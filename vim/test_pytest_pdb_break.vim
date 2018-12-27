if !has('unix')
	127cquit!
endif

let s:tempdir = $PYTEST_PDB_BREAK_TEST_TEMPDIR
if empty(s:tempdir) || s:tempdir !~# '^\%(/[^/]\+\)\{2,}'
	126cquit!
endif
let s:temphome = s:tempdir .'/vim'
call mkdir(s:temphome, 'p')

" Get autoload script's # (not ours)
function s:sid()
	return matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_runner.*$')
endfun
let s:g = g:pytest_pdb_break_overrides
let s:g.runner = funcref('s:sid')
let s:pfx = printf('<SNR>%s_', pytest_pdb_break#run())
let s:g.runner = s:g._orig.runner
let s:this_buffer = bufname('%')
let s:s = s:g._s
let s:errors = []

call assert_equal(expand('%:p:h') . '/autoload/pytest_pdb_break.vim',
			\ s:s.get('file'))
execute 'cd '. s:temphome


" Utils -----------------------------------------------------------------------

function s:_fmterrors(k, v) "{{{
	let pat = '\(^.*\)\(line \d\+\): \(.*\)'
	let F = {m -> printf("[%d] %s(%s)\n\t%s\n", a:k,
				\	fnamemodify(m[1], ':t'), m[2], m[3])}
	return substitute(a:v, pat, F, '')
endfunction "}}}

function s:_report() "{{{
	let s:errors += map(copy(v:errors), funcref('s:_fmterrors'))
	if has('nvim')
		echo join(s:errors, '')
	elseif exists('$VIMTEMP')
		execute 'redir > ' . $VIMTEMP
		echo join(s:errors, '')
		redir END
	endif
endfunction "}}}

function s:runfail(test_func, ...) "{{{
	" ... => [exit hander][defer]
	let ec = 100
	let Handler = a:0 ? a:1 : v:null
	let defer = a:0 == 2 ? a:2 : v:false
	try
		call a:test_func()
		let ec = 0
	catch /.*/
		call add(s:errors, v:exception)
		let m = matchlist(v:exception, '^Vim\%((\a\+)\)\=:E\(\d\+\)')
		let ec = get(m, 1, 101)
	finally
		if type(Handler) == 2
			try
				call Handler()
			catch /.*/
				call add(s:errors, v:exception)
				let ec = 102
			endtry
		endif
		if !empty(s:errors) && !defer
			echo join(s:errors, "\n") . "\n"
		endif
		if !empty(v:errors)
			let ec = len(v:errors)
			call s:_report()
		endif
		if ec && !defer
			execute ec .'cquit!'
		endif
	endtry
	return ec
endfunction "}}}

function s:pybuf(name) "{{{
	let tempname = s:temphome . '/' . a:name
	if a:name =~# '^test_'
		call delete(tempname, 'rf')
		call mkdir(tempname)
	endif
	let Func = funcref('s:'. a:name)
	execute 'cd '. tempname
	let scratchbuf = tempname . '/source.py'
	execute 'edit '. scratchbuf
	call assert_true(exists('*s:'. a:name))
	call assert_equal('python', &filetype)
	call assert_false(exists('b:pytest_pdb_break_context'))
	call assert_false(exists('b:pytest_pdb_break_python_exe'))
	function! s:_pybuf_handler() closure
		execute 'bdelete!'. scratchbuf
		call assert_false(bufloaded(tempname))
		call assert_equal(s:this_buffer, bufname('%'))
	endfunction
	call s:runfail(Func, funcref('s:_pybuf_handler'))
endfunction "}}}

function s:capture(func, ...) "{{{
	" ... => [logfile name][mode]
	let rv = v:null
	let extra = []
	redir => output
	try
		silent exec 'let rv = a:func()'
	catch /.*/
		let extra += [v:exception]
	finally
		redir END
		let outlines = ['<<< '. string(a:func)] + split(output, "\n") + extra
		let outname = a:0 && type(a:1) == 1 ?
					\ a:1 : bufname('%') =~# 'source\.py$' ?
					\ 'log' : fnamemodify(bufname('%'), ':r') . '.log'
		let mode = a:0 == 2 ? a:2 : 'a'
		call writefile(outlines, outname, mode)
	endtry
	return [rv, output] " exc not included but maybe should be
endfunction "}}}

function s:write_src(src) "{{{
	if bufname('%') !~# '^'. s:temphome
		throw 'Cannot write '. bufname('%')
	endif
	normal! dG
	call append(0, a:src)
	normal! dG
	call assert_equal(a:src[-1], getline('$'))
	silent write
endfunction "}}}

function s:test_fail(ecode)
	throw 'Should be '. a:ecode
endfunction
call assert_equal(101, s:runfail(funcref('s:test_fail', [101]), v:null, v:true))
call assert_equal(['Should be 101'], s:errors)
let s:errors = []
call assert_equal(102, s:runfail(
			\ function('acos', [-1]), funcref('s:test_fail', [102]), v:true
			\ ))
call assert_equal(['Should be 102'], s:errors)
let s:errors = []
call s:runfail(function('assert_true', [v:true])) " No file-scope v:errors yet

function s:test_utils()
	call assert_true(v:false)
endfunction
" Exit code matches len(v:errors)
call assert_equal(1, s:capture(
			\ funcref('s:runfail', [funcref('s:test_utils'), v:null, v:true]),
			\ s:temphome . '/test_utils.log', ''
			\ )[0])
let s:_soon = len(s:errors) == 1 && s:errors[0] =~# 'test_utils.*line 1'
			\ && len(v:errors) == 1 && v:errors[0] =~# 'test_utils.*line 1'
let s:errors = []
let v:errors = []
call assert_true(s:_soon)
unlet s:_soon


" is_custom -------------------------------------------------------------------

function s:test_is_custom() "{{{
	call assert_true(exists('*'. s:pfx .'is_custom'))
	let Ic = funcref(s:pfx . 'is_custom')
	let overrideables = [
				\ 'extend_python_path', 'get_context', 'get_node_id',
				\ 'query_helper', 'runner', 'split'
				\ ]
	for name in overrideables
		call assert_false(Ic(name), name .' has not been overridden')
	endfor
	" XXX why does this propagate (not handled by assert_fails)?
	try
		call assert_fails(call(Ic, ['is_custom']))
	catch /.*/
		call assert_exception('is_custom is not overrideable')
	endtry
	call assert_equal(overrideables, sort(keys(s:g._orig)))
	" Equiv exp below, otherwise would need closure over overrideables
	function! g:pytest_pdb_break_overrides.runner(...)
		call assert_true(exists('self'))
		call assert_equal(self.get_context, s:g._orig.get_context)
		call assert_equal(sort(keys(s:g._orig)),
					\ sort(filter(keys(self), 'v:val !~# "^_"')))
		call assert_equal(['foo', 'bar'], a:000)
		return v:true
	endfunction
	call assert_true(Ic('runner'))
	call assert_notequal(s:g._orig.runner, s:g.runner)
	call assert_true(g:pytest_pdb_break_overrides.runner('foo', 'bar'))
	let s:g.runner = s:g._orig.runner " reset
endfunction "}}}

call s:runfail(funcref('s:test_is_custom'))


" get_context -----------------------------------------------------------------

function s:test_get_context() "{{{
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
endfunction "}}}

call s:pybuf('test_get_context')
call assert_equal(s:g._orig.get_context, s:g.get_context)


" get_node_id -----------------------------------------------------------------

let s:src_two_funcs = [
			\ 'def test_first():',
			\ '    varone = True',
			\ '    assert varone',
			\ '',
			\ 'def some_func():',
			\ '    # a comment',
			\ '    return 1',
			\ '',
			\ 'def test_last(request):',
			\ '    vartwo = True',
			\ '    assert vartwo',
			\ ]

let s:src_one_class = [
			\ 'class TestBed:',
			\ '    def test_one(self):',
			\ '        varone = 1',
			\ '        assert varone',
			\ '',
			\ '    @deco',
			\ '    def test_two(self, request):',
			\ '        vartwo = 2',
			\ '        assert vartwo',
			\ ]

function s:_get_python_jump_prev() "{{{
	" Hijack built-in [m
	redir => l:cap
	silent! scriptnames
	redir END
	let pat = '\zs\d\+\ze: *' . $VIMRUNTIME . '/ftplugin/python.vim'
	let mstr = matchstr(l:cap, pat)
	let funcname = '<SNR>'. mstr .'_Python_jump'
	" Original b:prev pattern escapes <bar> because it's used in a mapping
	let args = ['n', '\v^\s*(class|def|async def)>', 'Wb']
	return funcref(funcname, args)
endfunction "}}}

function s:test_get_node_id_two_funcs() "{{{
	" Override
	let g:pytest_pdb_break_overrides.get_node_id = {-> a:000}
	call assert_equal([1, 2, 3], s:g._orig.get_node_id(1, 2, 3))
	let s:g.get_node_id = s:g._orig.get_node_id
	" Setup
	let buf = bufname('%')
	call assert_true(buf =~# '^/')
	let s:python_jump_prev = s:_get_python_jump_prev()
	call s:write_src(s:src_two_funcs)
	" Baseline
	call cursor(1, 1)
	let pos = searchpos('varone')
	call assert_notequal([0, 0], pos)
	call s:python_jump_prev()
	call assert_true(getline('.') =~# 'def test_first')
	" String
	call cursor(pos)
	let rv = s:g._orig.get_node_id()
	call assert_equal(join([buf, 'test_first'], '::'), rv)
	" External
	let ext_pos = searchpos('# a comment')
	call assert_notequal([0, 0], ext_pos)
	let [line_num, column] = pos
	let rv = s:g._orig.get_node_id(1, [0, line_num, column, 0])
	call assert_equal([buf, 'test_first'], rv)
	call assert_equal(ext_pos, getpos('.')[1:2])
	" List
	call cursor(pos)
	let rv = s:g._orig.get_node_id(1)
	call assert_equal([buf, 'test_first'], rv)
	call assert_equal(pos, getpos('.')[1:2])
	" In def line
	call cursor(1, 1)
	call assert_true(search('test_first') > 0)
	let rv = s:g._orig.get_node_id(1)
	call assert_equal([buf, 'test_first'], rv)
	" Last line
	call cursor(line('$'), 1)
	normal! $
	let rv = s:g._orig.get_node_id(1)
	call assert_equal([buf, 'test_last'], rv)
	" No match
	call cursor(ext_pos)
	let rv = s:capture(funcref(s:g._orig.get_node_id, [1]))[0]
	call assert_equal(-1, rv)
	call assert_equal(ext_pos, getpos('.')[1:2])
endfunction "}}}

function s:test_get_node_id_one_class() "{{{
	let buf = bufname('%')
	call assert_true(buf =~# '^/')
	call s:write_src(s:src_one_class)
	" Baseline
	call cursor(1, 1)
	let pos = searchpos('varone')
	call assert_notequal([0, 0], pos)
	call s:python_jump_prev()
	call assert_true(getline('.') =~# 'def test_one')
	" String
	call cursor(pos)
	let rv = s:g._orig.get_node_id()
	call assert_equal(buf .'::TestBed::test_one', rv)
	" With signature
	call setline(1, 'class TestBed(object):')
	call assert_equal(pos, getpos('.')[1:2])
	let rv = s:g._orig.get_node_id()
	call assert_equal(buf .'::TestBed::test_one', rv)
	" Last line
	call cursor(line('$'), 1)
	let rv = s:g._orig.get_node_id()
	call assert_equal(buf .'::TestBed::test_two', rv)
	" Between
	call cursor(1, 1)
	call assert_true(search('^$') > 0)
	let rv = s:g._orig.get_node_id()
	call assert_equal(buf .'::TestBed::test_one', rv)
endfunction "}}}

call s:pybuf('test_get_node_id_two_funcs')
call s:pybuf('test_get_node_id_one_class')
call assert_equal(s:g._orig.get_node_id, s:g.get_node_id)


" query_helper ----------------------------------------------------------------

function s:test_query_helper() "{{{
	let g:pytest_pdb_break_overrides.query_helper = {-> a:000}
	call assert_equal([{'a': 1}, 2, 3], s:g._orig.query_helper({'a': 1}, 2, 3))
	let s:g.query_helper = s:g._orig.query_helper
	" ERROR - no pytest (unrealistic since get_context() uses shebang)
	call assert_true(filereadable(s:s.get('helper')))
	let ctx = {'exe': s:tempdir . '/.venv_bare/bin/python'}
	call assert_equal(ctx.exe, exepath(ctx.exe), 'exepath mismatch')
	let [__, out] = s:capture(funcref(s:g.query_helper, [ctx]))
	call assert_match('Traceback.*ModuleNotFoundError', out)
	call assert_equal({'exe': s:tempdir . '/.venv_bare/bin/python'}, ctx)
	" Has pytest
	let ctx = {'exe': s:tempdir . '/.venv_base/bin/python'}
	call s:g.query_helper(ctx)
	call assert_false(ctx.registered)
	call assert_equal(getcwd(), ctx.rootdir)
	call assert_equal(s:tempdir . '/.venv_base/bin/python', ctx.exe)
	" ERROR - Bad option
	let ctx = {'exe': s:tempdir . '/.venv_base/bin/python'}
	let [__, out] = s:capture(funcref(s:g.query_helper, [ctx, '--fake']))
	call assert_match('unrecognized arguments', out)
	call assert_equal({'exe': s:tempdir . '/.venv_base/bin/python'}, ctx)
	" Node-id - nonexistent/unsaved file
	call assert_false(filereadable(bufname('%')))
	let ctx = {'exe': s:tempdir . '/.venv_base/bin/python'}
	call s:g.query_helper(ctx, bufname('%'))
	call assert_false(ctx.registered)
	call assert_equal(getcwd(), ctx.rootdir)
	" Node-id - nonexistent/unsaved file and unknown func
	let ctx = {'exe': s:tempdir . '/.venv_base/bin/python'}
	call s:g.query_helper(ctx, bufname('%') .'::test_fake')
	call assert_false(ctx.registered)
	call assert_equal(getcwd(), ctx.rootdir)
	" Node-id - real file, unknown func
	call s:write_src(s:src_two_funcs)
	let ctx = {'exe': s:tempdir . '/.venv_base/bin/python'}
	call s:g.query_helper(ctx, bufname('%') .'::test_fake')
	call assert_false(ctx.registered)
	call assert_equal(getcwd(), ctx.rootdir)
	" Node-id - real file, real func
	let ctx = {'exe': s:tempdir . '/.venv_base/bin/python'}
	call s:g.query_helper(ctx, bufname('%') .'::test_first')
	call assert_false(ctx.registered)
	call assert_equal(getcwd(), ctx.rootdir)
	" Alt rootdir (empty, not ancestor of CWD or node-id path)
	let ctx = {'exe': s:tempdir . '/.venv_base/bin/python'}
	let altroot = s:temphome .'/test_get_context'
	call s:g.query_helper(ctx, '--rootdir='. altroot, bufname('%'))
	call assert_false(ctx.registered)
	call assert_equal(altroot, ctx.rootdir)
	" Has plugin
	let ctx = {'exe': s:tempdir . '/.venv_self/bin/python'}
	call s:g.query_helper(ctx, bufname('%') .'::test_fake')
	call assert_true(ctx.registered)
	call assert_equal(getcwd(), ctx.rootdir)
	" With ini
	call writefile(['[pytest]', 'addopts = -v'], 'pytest.ini')
	let ctx = {'exe': s:tempdir . '/.venv_self/bin/python'}
	call s:g.query_helper(ctx, bufname('%') .'::test_fake')
	call assert_true(ctx.registered)
	call assert_equal(getcwd(), ctx.rootdir)
endfunction "}}}

call s:pybuf('test_query_helper')
call assert_equal(s:g._orig.query_helper, s:g.query_helper)


" extend_python_path ----------------------------------------------------------

function s:test_extend_python_path() "{{{
	" Basic vim reminders
	let $PYTHONPATH = ''
	call assert_false(exists('$PYTHONPATH'))
	call assert_true(empty($PYTHONPATH))
	"
	let ctx = {'PP': '/tmp/fake'}
	call assert_equal(ctx.PP, s:g.extend_python_path(ctx))
	unlet ctx.PP
	let home = s:s.get('home')
	call assert_true(filereadable(home . '/tox.ini'))
	call assert_equal(home, s:g.extend_python_path(ctx))
	call assert_equal(ctx.PP, home)
	unlet ctx.PP
	"
	let first = s:temphome . '/first'
	let $PYTHONPATH = first
	call assert_equal(home .':'. first, s:g.extend_python_path(ctx))
	call assert_equal(home .':'. first, ctx.PP)
	" No uniq-like filtering
	let $PYTHONPATH = ctx.PP
	unlet ctx.PP
	let expected = join([home, home, first], ':')
	call assert_equal(expected, s:g.extend_python_path(ctx))
	call assert_equal(expected, ctx.PP)
	let $PYTHONPATH = ''
endfunction "}}}

call s:runfail(funcref('s:test_extend_python_path'))

" -----------------------------------------------------------------------------
quitall!
