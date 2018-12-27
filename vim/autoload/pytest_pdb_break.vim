scriptencoding utf-8

let s:file = expand('<sfile>')

function! pytest_pdb_break#run(...) abort
	return call('s:runner', a:000)
endfunction


function! s:is_custom(name) abort "{{{
	if !has_key(s:defuncs._orig, a:name)
		throw a:name .' is not overrideable'
	endif
	if get(g:pytest_pdb_break_overrides, a:name) == funcref('s:' . a:name)
		return 0
	endif
	return 1
endfunction "}}}

function! s:get_context() abort "{{{
	" Save venv info in buffer-local dict, keyed by python executable
	if s:is_custom('get_context')
		return g:pytest_pdb_break_overrides.get_context()
	endif
	call assert_equal(&filetype, 'python')
	if !exists('s:home')
		let path = fnamemodify(s:file, ':h:p') . ';'
		let plugin = findfile('pytest_pdb_break.py', path)
		if empty(plugin)
			throw 'Could not find plugin root above entry in &rtp'
		endif
		let s:plugin = fnamemodify(plugin, ':p') " covers the rare './plugin'
		let s:home = fnamemodify(s:plugin, ':h:p') " no trailing/
		let s:helper = simplify(s:home . '/get_config_info.py')
	endif
	if !exists('b:pytest_pdb_break_context')
		let b:pytest_pdb_break_context = {}
	endif
	if exists('b:pytest_pdb_break_python_exe')
		let exe = b:pytest_pdb_break_python_exe
	else
		try
			let shebang = readfile(exepath('pytest'), '', 1)[0]
			let exe = substitute(shebang, '^#!', '', '')
		catch /^Vim\%((\a\+)\)\=:E/
			let exe = ''
		endtry
		if !executable(exe)
			let exe = exepath('python')
		endif
	endif
	if !has_key(b:pytest_pdb_break_context, exe)
		let b:pytest_pdb_break_context[exe] = {'exe': exe}
	endif
	return b:pytest_pdb_break_context[exe]
endfunction "}}}

function! s:report_failed(matchgroup) abort "{{{
	echohl WarningMsg | echo 'Search failed' | echohl Normal
	echo ' line: ' . getline('.')
	echo ' match: ' . string(a:matchgroup)
endfunction "}}}

function! s:get_node_id(...) abort "{{{
	" Return a pytest node-id string or, with varg 1, its components as a list.
	" Varg 2, if provided, is the start pos.
	if s:is_custom('get_node_id')
		return call(g:pytest_pdb_break_overrides.get_node_id, a:000)
	endif
	let saved_cursor = getcurpos()
	try
		if a:0 == 2
			call setpos('.', a:2)
		endif
		normal! $
		call search('\v^\s*(class|def|async def)>', 'Wb')
		let pat = '^\s*\(def\|async def\)\s\(test_\w\+\)(\(.*\)).*$'
		let groups = matchlist(getline('.'), pat)
		if !len(groups) || empty(groups[2])
			call s:report_failed(groups)
			return -1
		endif
		let nodeid = [groups[2]]
		if groups[3] =~# '^self.*' && search('\_^\s*class\s', 'Wb') != 0
			let maybeclass = matchlist(getline('.'), '^\s*class\s\(\w\+\).*:.*')
			if empty(maybeclass) || empty(maybeclass[1])
				call s:report_failed(maybeclass)
			endif
			if !empty(maybeclass) && !empty(maybeclass[1])
				call add(nodeid, maybeclass[1])
			endif
		endif
	finally
		call setpos('.', saved_cursor)
	endtry
	call add(nodeid, expand('%:p'))
	return a:0 && a:1 ? reverse(nodeid) : join(reverse(nodeid), '::')
endfunction "}}}

function! s:query_helper(ctx, ...) abort "{{{
	" https://docs.pytest.org/en/latest/customize.html#finding-the-rootdir
	if s:is_custom('query_helper')
		return call(g:pytest_pdb_break_overrides.query_helper, [a:ctx] + a:000)
	endif
	let context = a:ctx
	let cmdline = [context.exe, '-'] + a:000
	if !has('nvim')
		let cmdline = join(cmdline)
	endif
	let lines = readfile(s:helper)
	try
		let result = system(cmdline, lines)
		try
			let decoded = json_decode(result)
		catch /^Vim\%((\a\+)\)\=:E474/
			throw result
		endtry
		call extend(context, decoded)
	catch /.*/
		echohl WarningMsg | echo 'Problem calling helper' | echohl None
		echo 'path: '. s:helper
		echo 'cmdline: '. string(cmdline)
		" Truncation often lops off final exc (most recent call last)
		if exists('result') && stridx(v:exception, result) == -1
					\ && v:exception =~# 'E474.*Traceback'
			echo 'exc: '. matchstr(v:exception, '.*\zeTraceback')
			echo result
		else
			echo 'exc: '. v:exception
		endif
		echoerr 'HelperError'
	endtry
endfunction "}}}

function! s:extend_python_path(ctx) abort "{{{
	" Stash modified copy of PYTHONPATH, should be unset if unneeded
	if s:is_custom('extend_python_path')
		return g:pytest_pdb_break_overrides.extend_python_path(a:ctx)
	endif
	let ctx = a:ctx
	if has_key(ctx, 'PP')
		return ctx.PP
	endif
	let val = empty($PYTHONPATH) ? [] : split($PYTHONPATH, ':')
	call insert(val, s:home)
	let ctx.PP = join(val, ':')
	return ctx.PP
endfunction "}}}

function! s:runner(...) abort "{{{
	if s:is_custom('runner')
		return call(g:pytest_pdb_break_overrides.runner, a:000)
	endif
	let ctx = s:get_context()
	let nid = s:get_node_id(1)
	let cmd = [ctx.exe, '-mpytest']
	let arg = join(nid, '::')
	let opts = []
	let last = get(ctx, 'last', {})
	if !has_key(ctx, 'registered') || a:000 != get(last, 'uopts', ["\u2049"])
		try
			call call('s:query_helper', [ctx] + a:000 + [arg])
		catch /.*HelperError/
			return
		endtry
	endif
	if !ctx.registered
		let preenv = s:extend_python_path(ctx)
		let cmd = ['env', printf('PYTHONPATH=%s', preenv)] + cmd
		let opts = ['-p', 'pytest_pdb_break']
	endif
	call add(opts,  printf('--break=%s:%s', nid[0], line('.')))
	let ctx.last = {'cmd': cmd, 'uopts': a:000, 'opts': opts, 'node_id': nid}
	return call('s:split', cmd + a:000 + opts + [arg])
endfunction "}}}

function! s:split(...) abort "{{{
	if s:is_custom('split')
		return call(g:pytest_pdb_break_overrides.split, a:000)
	endif
	let context = s:get_context()
	let vert = (winwidth(0) + 0.0) / winheight(0) > 2.5
	if has('nvim')
		execute vert ? 'vnew' : 'new'
		let cwd =  {'cwd': get(context, 'rootdir', expand('%:p:h'))}
		let context.job = termopen(a:000, cwd)
		let @@ = winnr()
		execute bufwinnr(bufnr('%')) . 'wincmd w'
		execute @@ . 'wincmd w'
		redraw | startinsert
	else
		let context.job = term_start(a:000, {'vertical': vert})
	endif
endfunction "}}}


let s:defuncs = {'get_context': funcref('s:get_context'),
				\ 'query_helper': funcref('s:query_helper'),
				\ 'extend_python_path': funcref('s:extend_python_path'),
				\ 'get_node_id': funcref('s:get_node_id'),
				\ 'runner': funcref('s:runner'),
				\ 'split': funcref('s:split')}
let s:defuncs._orig = copy(s:defuncs)
let g:pytest_pdb_break_overrides._s = {
			\ 'exists': {e -> exists(e)},
			\ 'get': {v -> eval('s:'. v)}
			\ }
call extend(g:pytest_pdb_break_overrides, s:defuncs, 'keep')
