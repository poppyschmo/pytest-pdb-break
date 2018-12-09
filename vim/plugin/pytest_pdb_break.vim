
" TODO make this work with regular Vim
if ! has("nvim")
	finish
endif

" Any args are passed on to pytest, for example: :PytestBreakHere -o addopts=
if exists(':PytestBreakHere') != 2
	command! -nargs=* PytestBreakHere call pytest_pdb_break#run(<f-args>)
endif

