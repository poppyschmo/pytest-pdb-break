
" Get autoload script's # (not ours)
function! s:sid()
	return matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_runner.*$')
endfun
let s:g = g:pytest_pdb_break_overrides
let s:g.runner = funcref('s:sid')
let s:pfx = printf('<SNR>%s_', pytest_pdb_break#run())
let s:g.runner = s:g._orig.runner
let s:this_buffer = bufname('%')
let s:s = s:g._s
call assert_equal(expand('%:p:h') . '/autoload/pytest_pdb_break.vim',
			\ s:s.get('file'))

function! s:pybuf(f)
	let tempname = tempname() . '.py'
	execute 'edit '. tempname
	call assert_equal('python', &filetype)
	call assert_false(exists('b:pytest_pdb_break_context'))
	call assert_false(exists('b:pytest_pdb_break_python_exe'))
	try
		call a:f()
	finally
		execute bufnr(tempname) .'bdelete!'
		call assert_false(bufloaded(tempname))
	endtry
endfunction


" is_custom -------------------------------------------------------------------

call assert_true(exists('*'. s:pfx .'is_custom'))
let s:is_custom = funcref(s:pfx . 'is_custom')

let s:overrideables = [
			\ 'check_plugin', 'extend_python_path',
			\ 'get_context', 'get_node_id',
			\ 'runner', 'split'
			\ ]
call assert_equal(s:overrideables, sort(keys(s:g._orig)))

for name in s:overrideables
	call assert_false(s:is_custom(name), name .' has not been overridden')
endfor

" XXX why does this propagate (not handled by assert_fails)?
try
	call assert_fails(call(s:is_custom, ['get_python_jump_prev']))
catch /.*/
	call assert_exception('get_python_jump_prev is not overrideable')
endtry

function! g:pytest_pdb_break_overrides.runner(...)
	call assert_true(exists('self'))
	call assert_equal(self.get_context, s:g._orig.get_context)
	call assert_equal(s:overrideables,
				\ sort(filter(keys(self), 'v:val !~# "^_"')))
	call assert_equal(['foo', 'bar'], a:000)
	return v:true
endfunction

call assert_true(s:is_custom('runner'))
call assert_notequal(s:g._orig.runner, s:g.runner)
call assert_true(g:pytest_pdb_break_overrides.runner('foo', 'bar'))
let s:g.runner = s:g._orig.runner " reset


" get_python_jump_prev --------------------------------------------------------

let s:get_python_jump_prev = funcref(s:pfx . 'get_python_jump_prev')
try
	call assert_fails(s:get_python_jump_prev(), 'No python ftplugin in rtp')
catch /.*/
	call assert_exception('No python ftplugin in rtp')
endtry

function s:t_get_python_jump_prev()
	call assert_equal(2, type(s:get_python_jump_prev()))
endfunction
call s:pybuf(funcref('s:t_get_python_jump_prev'))


" get_context -----------------------------------------------------------------

function s:t_get_context()
	call assert_false(s:s.exists('s:plugin'))
	call assert_false(s:s.exists('s:home'))
	call assert_false(s:s.exists('s:helper'))
	call assert_false(exists('b:pytest_pdb_break_context'))
	call s:g.get_context()
	call assert_true(s:s.exists('s:plugin'))
	call assert_true(s:s.exists('s:home'))
	call assert_true(s:s.exists('s:helper'))
	call assert_true(exists('b:pytest_pdb_break_context'))
	call assert_equal(fnamemodify(s:s.get('file'), ':h:h:h'), s:s.get('home'))
	let syspy = exepath('python')
	call assert_equal({'exe': syspy}, b:pytest_pdb_break_context[syspy])
	" Script-local vars persist
	let before = map(['plugin', 'home', 'helper'], 's:s.get(v:val)')
	let b:pytest_pdb_break_python_exe = '/tmp/fakepython'
	call s:g.get_context()
	call assert_true(has_key(b:pytest_pdb_break_context, '/tmp/fakepython'))
	let after = map(['plugin', 'home', 'helper'], 's:s.get(v:val)')
	call assert_equal(before, after)
endfunction
call s:pybuf(funcref('s:t_get_context'))
call assert_equal(s:g._orig.get_context, s:g.get_context)


" get_node_id -----------------------------------------------------------------

let g:pytest_pdb_break_overrides.get_node_id = {-> a:000}
call assert_equal([1, 2, 3], s:g._orig.get_node_id(1, 2, 3))
let s:g.get_node_id = s:g._orig.get_node_id


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
	elseif exists('$VIMTEMP')
		execute 'redir > ' . $VIMTEMP
		echo join(s:errors, '')
		redir END
	endif
	cquit!
endif

quitall!

