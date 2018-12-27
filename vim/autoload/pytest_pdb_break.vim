let s:file = expand('<sfile>')

function! pytest_pdb_break#run(...)
	return call('s:runner', a:000)
endfunction

function! pytest_pdb_break#toggle(...)
	let context = s:get_context()
	let context.registered = a:0 ? a:1 : !get(context, 'registered', 0)
	echo 'Plugin injection: '. (context.registered ? 'off' : 'on')
endfunction

function! s:get_context() "{{{
	" Save venv info in buffer-local dict, keyed by python executable
	if exists('g:pytest_pdb_break_overrides.get_context')
		return call(g:pytest_pdb_break_overrides.get_context)
	endif
	if !exists('s:home')
		let path = fnamemodify(s:file, ':h:p') . ';'
		let plugin = findfile('pytest_pdb_break.py', path)
		if plugin == ''
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
			let exe = substitute(shebang, '^!#', '', '')
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

function! s:check_plugin(ctx, ...) "{{{
	if exists('g:pytest_pdb_break_overrides.check_plugin')
		return call(g:pytest_pdb_break_overrides.check_plugin, [a:ctx, a:000])
	endif
	let context = a:ctx
	if !has_key(context, 'registered')
		" Heard somewhere that using job control instead of system() can help
		" with shell-quoting issues. No idea.
		if has('nvim')
			let jd = {'stdout_buffered': v:true, 'stderr_buffered': v:true}
			let lines = readfile(s:helper)
			let jd.id = jobstart([context.exe, '-'] + a:000, jd)
			let jd.sent = chansend(jd.id, lines)
			call chanclose(jd.id, 'stdin')
			let jd.exit = jobwait([jd.id], 1000)[0]
			if jd.exit
				let context.helper_rv = jd
				echo join(jd.stderr, "\n")
				throw "Error querying pytest"
			endif
			call extend(context, json_decode(jd.stdout[0]))
		else
			" call ch_logfile('channellog', 'a')
			let jd = {'in_io': 'file', 'in_name': s:helper}
			let job = job_start([context.exe, '-'] + a:000, jd)
			let jd.stdout = ch_readraw(job)
			call extend(jd, job_info(job))
			if jd.exitval || jd.stdout == ""
				let context.helper_rv = jd
				echo ch_readraw(job, {"part": "err"})
				throw "Error querying pytest"
			endif
			call extend(context, json_decode(jd.stdout))
		endif
	endif
	return context.registered
endfunction "}}}

function! s:extend_python_path(ctx) "{{{
	" Stash modified copy of PYTHONPATH, should be unset if unneeded
	if exists('g:pytest_pdb_break_overrides.extend_python_path')
		return call(g:pytest_pdb_break_overrides.extend_python_path, a:ctx)
	endif
	let context = a:ctx
	if has_key(context, 'PP')
		return context.PP
	endif
	let val = expand('$PYTHONPATH')
	let val = val == '$PYTHONPATH' ? [] : split(val, ':')
	call add(val, fnameescape(s:home))
	let context.PP = join(val, ':')
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
	call add(nodeid, expand('%:p'))
	let context = s:get_context()
	let args = []
	let cmd = [context.exe, '-mpytest']
	let arg = join(reverse(nodeid), "::")
	let registered = call('s:check_plugin', [context] + a:000 + [arg])
	if !registered
		let preenv = s:extend_python_path(context)
		let cmd = ['env', printf('PYTHONPATH=%s', preenv)] + cmd
		call extend(args, ['-p', 'pytest_pdb_break'])
	endif
	let opt = printf('--break=%s:%s', expand('%:p'), line('.'))
	call extend(args, [opt, arg])
	let context.last = cmd + a:000 + args
	return call('s:split', context.last)
endfunction "}}}

function! s:split(...) "{{{
	if exists('g:pytest_pdb_break_overrides.split')
		return call(g:pytest_pdb_break_overrides.split, a:000)
	endif
	let context = s:get_context()
	let vert = (winwidth(0) + 0.0) / winheight(0) > 2.5
	if has('nvim')
		execute vert ? 'vnew' : 'new'
		let context.job = termopen(a:000, {'cwd': context.rootdir})
		let @@ = winnr()
		execute bufwinnr(bufnr('%')) . 'wincmd w'
		execute @@ . 'wincmd w'
		redraw | startinsert
	else
		let context.job = term_start(a:000, {'vertical': vert})
	endif
endfunction "}}}
