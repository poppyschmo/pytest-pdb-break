" Try with: Plug 'poppyschmo/pytest-pdb-break', { 'rtp': 'vim' }

if exists('g:loaded_pytest_pdb_break')
	finish
endif

let g:loaded_pytest_pdb_break = 1

" Your func here:
"
"   function! s:mysplit(...)
"     echo 'Woulda run: ' . string(a:000)
"   endfunction
"
" let g:pytest_pdb_break_overrides.split = funcref('s:mysplit')
let g:pytest_pdb_break_overrides = {}

" Any args are passed on to pytest, for example: :PytestBreakHere -o addopts=
if exists(':PytestBreakHere') != 2
	command! -nargs=* PytestBreakHere call pytest_pdb_break#run(<f-args>)
endif

