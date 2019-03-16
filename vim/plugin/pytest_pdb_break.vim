
if exists('g:loaded_pytest_pdb_break')
  finish
endif

let g:loaded_pytest_pdb_break = 1
let g:pytest_pdb_break_overrides = {}
let g:pytest_pdb_break_extra_opts = []
let g:pytest_pdb_break_defaults = ['--complete']

command! -nargs=* PytestBreakHere call pytest_pdb_break#run(<f-args>)

