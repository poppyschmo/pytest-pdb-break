" Install the usual way:
"
"   Plug 'poppyschmo/pytest-pdb-break', { 'rtp': 'vim' }

if exists('g:loaded_pytest_pdb_break')
	finish
endif

let g:loaded_pytest_pdb_break = 1
let g:pytest_pdb_break_overrides = {}

" Any args are passed on to pytest, for example: :PytestBreakHere -o addopts=
if exists(':PytestBreakHere') != 2
	command! -nargs=* PytestBreakHere call pytest_pdb_break#run(<f-args>)
endif

