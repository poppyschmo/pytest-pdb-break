
" Get autoload script's # (not ours)
function! s:sid()
	return matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_runner.*$')
endfun
let g:pytest_pdb_break_overrides.runner = funcref('s:sid')
let s:pfx = printf('<SNR>%s_', pytest_pdb_break#run())
let s:this_buffer = bufname('%')


" is_custom -------------------------------------------------------------------

call assert_true(exists('*'. s:pfx .'is_custom'))
let s:is_custom = funcref(s:pfx . 'is_custom')

let s:overrideables = [
			\ 'get_context', 'check_plugin', 'get_node_id',
			\ 'split', 'extend_python_path'
			\ ]

for name in s:overrideables
	call assert_false(s:is_custom(name), name .' has not been overridden')
endfor

call assert_true(s:is_custom('runner'))
call add(s:overrideables, 'runner')
call sort(s:overrideables)


" XXX why does this propagate (not handled by assert_fails)?
try
	call assert_fails(call(s:is_custom, ['get_python_jump_prev']))
catch /.*/
	call assert_exception('get_python_jump_prev is not overrideable')
endtry

function! g:pytest_pdb_break_overrides.runner(...)
	call assert_true(exists('self'))
	call assert_equal(self.get_context,
				\ g:pytest_pdb_break_overrides.get_context)
	call assert_equal(s:overrideables,
				\ sort(filter(keys(self), 'v:val != "_orig"')))
	call assert_equal(['foo', 'bar'], a:000)
	return v:true
endfunction

call assert_notequal(g:pytest_pdb_break_overrides._orig.runner,
			\ g:pytest_pdb_break_overrides.runner)

call assert_true(g:pytest_pdb_break_overrides.runner('foo', 'bar'))


" get_python_jump_prev --------------------------------------------------------

let s:get_python_jump_prev = funcref(s:pfx . 'get_python_jump_prev')
try
	call assert_fails(s:get_python_jump_prev(), 'No python ftplugin in rtp')
catch /.*/
	call assert_exception('No python ftplugin in rtp')
endtry
let s:tempname = tempname()
execute 'edit '. s:tempname .'.py'
call assert_equal('python', &filetype,)
call assert_equal(2, type(s:get_python_jump_prev()))


" Report ----------------------------------------------------------------------

function s:fmterrors(k, v)
	let pat = '\(^.*\)\(line \d\+\): \(.*\)'
	let F = {m -> printf("[%d] %s(%s)\n\t%s\n", a:k,
				\	fnamemodify(m[1], ':t'), m[2], m[3])}
	return substitute(a:v, pat, F, '')
endfunction

if len(v:errors)
	let s:errors = map(v:errors, funcref('s:fmterrors'))
	if has('nvim')
		echo join(s:errors, '')
	else
		execute 'redir > ' . $VIMTEMP
		echo join(s:errors, '')
		redir END
	endif
	cquit!
endif

quitall!

