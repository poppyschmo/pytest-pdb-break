let s:file = expand('<sfile>')

function! pytest_pdb_break#run(...)
	let arg = printf("--break=%s:%s", expand('%'), line('.'))
	return call("s:runner", [arg] + a:000)
endfunction

function! pytest_pdb_break#toggle(...)
	let context = s:get_context()
	let context.found = a:0 ? a:1 : !get(context, 'found', 0)
	echo 'Plugin injection: '. (context.found ? 'off' : 'on')
endfunction

function! s:get_context() "{{{
	" Save venv info in buffer-local dict, keyed by python executable;
	" members are 'found' (always set), 'PP' (when not found), 'job' (id)
	if exists('g:pytest_pdb_break_overrides.get_context')
		return call(g:pytest_pdb_break_overrides.get_context)
	endif
	if ! exists('b:pytest_pdb_break_context')
		let b:pytest_pdb_break_context = {}
	endif
	let exe = exepath('python')
	if !has_key(b:pytest_pdb_break_context, exe)
		let b:pytest_pdb_break_context[exe] = {}
	endif
	return b:pytest_pdb_break_context[exe]
endfunction "}}}

function! s:check_plugin(...) "{{{
	if exists('g:pytest_pdb_break_overrides.check_plugin')
		return call(g:pytest_pdb_break_overrides.check_plugin, a:000)
	endif
	let context = a:0 ? a:1 : s:get_context()
	if !has_key(context, 'found')
		" TODO allow setting explicitly via some option
		let job = jobstart(['python', '-c', 'import pytest_pdb_break'])
		let context.found = jobwait([job], 100)[0] == 0
	endif
	return context.found
endfunction "}}}

function! s:extend_python_path(...) "{{{
	" Stash modified copy of PYTHONPATH, should be unset if unneeded
	if exists('g:pytest_pdb_break_overrides.extend_python_path')
		return call(g:pytest_pdb_break_overrides.extend_python_path, a:000)
	endif
	let context = a:0 ? a:1 : s:get_context()
	if has_key(context, 'PP')
		return context.PP
	endif
	let components = expand('$PYTHONPATH')
	if components == '$PYTHONPATH'
		let components = []
	else
		let components = split(components, ':')
	endif
	let found = findfile('pytest_pdb_break.py', fnamemodify(s:file, ':h:p') . ';')
	if found == ''
		throw 'Could not find plugin'
	endif
	call add(components, fnameescape(fnamemodify(found, ':h')))
	let context.PP = join(components, ':')
	return context.PP
endfunction "}}}

function! s:report_failed(matchgroup) "{{{
	echohl WarningMsg | echo "Search failed" | echohl Normal
	echo ' line: ' . getline(".")
	echo ' match: ' . string(a:matchgroup)
endfunction "}}}

function! s:runner(...) abort "{{{
	if exists('g:pytest_pdb_break_overrides.runner')
		return call(g:pytest_pdb_break_overrides.runner, a:000)
	endif
	let saved_cursor = getcurpos()
	normal! +
	normal [m
	let pat = '^\s*\(def\|async def\)\s\(test_\w\+\)(\(.*\)).*$'
	let groups = matchlist(getline("."), pat)
	if !len(groups) || groups[2] == ""
		call s:report_failed(groups)
		return -1
	endif
	let nodeid = [groups[2]]
	if groups[3] =~ "^self.*" && search('\_^\s*class\s', "Wb") != 0
		let maybeclass = matchlist(getline('.'), '^\s*class\s\(\w\+\).*:.*')
		if ! len(maybeclass) || maybeclass[1] == ""
			call s:report_failed(maybeclass)
		endif
		if len(maybeclass) && maybeclass[1] != ""
			call add(nodeid, maybeclass[1])
		endif
	endif
	call setpos('.', saved_cursor)
	call add(nodeid, expand('%'))
	let args = a:000 + [join(reverse(nodeid), "::")]
	let cmd = ['pytest']
	let context = s:get_context()
	if !s:check_plugin(context)
		let preenv = s:extend_python_path(context)
		let cmd = ['env', printf('PYTHONPATH=%s', preenv)] + cmd
		let args = ['-p', 'pytest_pdb_break'] + args
	endif
	return call('s:split', cmd + args)
endfunction "}}}

function! s:split(...) "{{{
	if exists('g:pytest_pdb_break_overrides.split')
		return call(g:pytest_pdb_break_overrides.split, a:000)
	endif
	let context = s:get_context()
	execute ((winwidth(0) + 0.0) / winheight(0) > 2.5) ? 'vnew' : 'new'
	let context.job = termopen(a:000)
	let @@ = winnr()
	execute bufwinnr(bufnr('%')) . 'wincmd w'
	execute @@ . 'wincmd w'
	redraw | startinsert
endfunction "}}}
