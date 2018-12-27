scriptencoding utf-8

let s:file = expand('<sfile>')

function! pytest_pdb_break#run(...) abort
	return s:call('runner', a:000, 1)
endfunction

function! s:call(name, args, ...) abort "{{{
	" ... => [clear]
	let d = a:0 && a:1 ?
				\ {} : get(g:pytest_pdb_break_overrides, '_flattened', {})
	if empty(d)
		let d = filter(copy(g:pytest_pdb_break_overrides), 'v:key !~# "^_"')
		call extend(d, s:defuncs, 'keep')
		let g:pytest_pdb_break_overrides._flattened = d
	endif
	return call(d[a:name], a:args, d)
endfunction "}}}

function! s:get_context() abort "{{{
	" Save venv info in buffer-local dict, keyed by python executable
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

function! s:_search_back(pat, test) abort "{{{
	let dent = indent('.')
	while 1
		if !search(a:pat, 'Wb')
			return 0
		endif
		let sid = synID(line('.'), col('.'), 1)
		if synIDattr(sid, 'name') !~? 'comment\|string' && a:test(dent)
			break
		endif
	endwhile
	return line('.')
endfunction "}}}

function! s:get_node_id(...) abort "{{{
	" ... => [want list][start pos]
	let spos = getcurpos()
	try
		if a:0 == 2
			call setpos('.', a:2)
		endif
		normal! $
		let gr = []
		if s:_search_back('\v^\s*(def|async def)>', {l -> indent('.') <= l})
			let pat = '^\s*\(def\|async def\)\s\(test_\w\+\)(\(.*\)).*$'
			let gr = matchlist(getline('.'), pat)
		endif
		if empty(gr) || empty(gr[2])
			echohl WarningMsg | echo 'No test found!' | echohl Normal
			echo printf(' beg(%d): %s', spos[1], shellescape(getline(spos[1])))
			echo printf(' end(%d): %s', line('.'), shellescape(getline('.')))
			echo ' match: ' . string(gr)
			throw 'Search failed'
		endif
		let nodeid = [gr[2]]
		if gr[3] =~# '^self.*' &&
					\ s:_search_back('\_^\s*class\s', {l -> indent('.') < l})
			let cgr = matchlist(getline('.'), '^\s*class\s\(\w\+\).*:.*')
			call add(nodeid, cgr[1])
		endif
	finally
		call setpos('.', spos)
	endtry
	call add(nodeid, expand('%:p'))
	return a:0 && a:1 ? reverse(nodeid) : join(reverse(nodeid), '::')
endfunction "}}}

function! s:query_helper(ctx, ...) abort "{{{
	" https://docs.pytest.org/en/latest/customize.html#finding-the-rootdir
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
	let ctx = s:call('get_context', [])
	let nid = s:call('get_node_id', [1])
	let cmd = [ctx.exe, '-mpytest']
	let arg = join(nid, '::')
	let opts = []
	let last = get(ctx, 'last', {})
	if !has_key(ctx, 'registered') || a:000 != get(last, 'uopts', ["\u2049"])
		try
			call s:call('query_helper', [ctx] + a:000 + [arg])
		catch /.*HelperError/
			return
		endtry
	endif
	let jd = {'cwd': ctx.rootdir}
	if !ctx.registered
		let preenv = s:call('extend_python_path', [ctx])
		if has('nvim')
			let cmd = ['env', printf('PYTHONPATH=%s', preenv)] + cmd
		else
			let jd.env = {'PYTHONPATH': preenv}
		endif
		let opts = ['-p', 'pytest_pdb_break']
	endif
	call add(opts,  printf('--break=%s:%s', nid[0], line('.')))
	let ctx.last = {
				\ 'cmd': cmd, 'uopts': a:000, 'opts': opts,
				\ 'node_id': nid, 'jd': jd
				\ }
	return s:call('split', [cmd + a:000 + opts + [arg], jd])
endfunction "}}}

function! s:split(cmdl, jobd) abort "{{{
	if !has_key(a:jobd, 'vertical')
		if exists('b:pytest_pdb_break_vertical')
			let a:jobd.vertical = b:pytest_pdb_break_vertical
		else
			let wn = winnr()
			let a:jobd.vertical = (winwidth(wn) + 0.0) / winheight(wn) > 2.5
		endif
	endif
	if has('nvim')
		execute a:jobd.vertical ? 'vnew' : 'new'
		let a:jobd.job = termopen(a:cmdl, a:jobd)
		startinsert
	else
		let a:jobd.job = term_start(a:cmdl, a:jobd)
	endif
endfunction "}}}


let s:defuncs = {'get_context': funcref('s:get_context'),
				\ 'query_helper': funcref('s:query_helper'),
				\ 'extend_python_path': funcref('s:extend_python_path'),
				\ 'get_node_id': funcref('s:get_node_id'),
				\ 'runner': funcref('s:runner'),
				\ 'split': funcref('s:split')}

if exists('g:pytest_pdb_break_testing')
	let g:pytest_pdb_break_testing.s = {
				\ 'exists': {e -> exists(e)},
				\ 'get': {v -> eval('s:'. v)}
				\ }
	let g:pytest_pdb_break_testing.o = copy(s:defuncs)
endif
