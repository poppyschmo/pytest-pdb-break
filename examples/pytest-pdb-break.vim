
" TODO make this work with regular Vim
if ! has("nvim")
	finish
endif

" Any args are passed on to pytest, for example: :PytestBreakHere -o addopts=
if exists(':PytestBreakHere') != 2
	command! -nargs=* PytestBreakHere call <SID>pytest_break_runner(<f-args>)
endif

function! s:pytest_break_runner(...)
	let arg = printf("--break=%s:%s", expand('%'),  line('.'))
	return call("s:pytest_runner", [arg] + a:000)
endfunction

function! s:report_failed(matchgroup)
	echohl WarningMsg | echo "Search failed" | echohl Normal
	echo ' line: ' . getline(".")
	echo ' match: ' . string(a:matchgroup)
endfunction

function! s:pytest_runner(...) abort "{{{
	let saved_cursor = getcurpos()
	normal [m
	let pat = '^\s*\(def\|async def\)\s\(test_\w\+\)(\(.*\)).*$'
	let groups = matchlist(getline("."), pat)
	if ! len(groups) || groups[2] == ""
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
	let cmd = ['pytest'] + a:000 + [join(reverse(nodeid), "::")]
	return s:run_split(cmd)
endfunction "}}}

function! s:run_split(cmd) "{{{
	" 'cmd' is list or string
	execute (( winwidth(0) + 0.0) / winheight(0) > 2.5 ) ? 'vnew' : 'new'
	let job1 = termopen(a:cmd)
	let @@ = winnr()
	execute bufwinnr(bufnr('%')) . 'wincmd w'
	let b:job1 = job1
	execute @@ . 'wincmd w'
	redraw | startinsert
endfunction "}}}
