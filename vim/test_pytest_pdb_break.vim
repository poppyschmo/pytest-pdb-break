scriptencoding utf-8

if !has('unix')
	cquit!
endif

if exists('$VIRTUAL_ENV')
	cquit!
endif

let s:tempdir = $PYTEST_PDB_BREAK_TEST_TEMPDIR
if empty(s:tempdir) || s:tempdir !~# '^\%(/[^/]\+\)\{2,}'
	cquit!
endif
let s:temphome = s:tempdir .'/vim'
call mkdir(s:temphome, 'p')

let g:pytest_pdb_break_testing = {}
let s:g = g:pytest_pdb_break_overrides
let s:g.runner = {-> matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_call.*$')}
let s:pfx = printf('<SNR>%s_', pytest_pdb_break#run())
let s:this_buffer = bufname('%')
let s:s = g:pytest_pdb_break_testing.s
let s:o = g:pytest_pdb_break_testing.o
let s:errors = []

if expand('%:p:h') . '/autoload/pytest_pdb_break.vim' != s:s.get('file')
	cquit! " bad &rtp or cwd or s:pfx
endif
execute 'cd '. s:temphome


function s:_fmterrors(k, v) "{{{
	let pat = '\(^.*\)\(line \d\+\): \(.*\)'
	let F = {m -> printf("[%d] %s(%s)\n\t%s\n", a:k,
				\	fnamemodify(m[1], ':t'), m[2], m[3])}
	return substitute(a:v, pat, F, '')
endfunction "}}}

function s:_report() "{{{
	let s:errors += map(copy(v:errors), funcref('s:_fmterrors'))
	if !empty($PYTEST_PDB_BREAK_TEST_VIM_TEST_OUTPUT)
		call writefile(s:errors, $PYTEST_PDB_BREAK_TEST_VIM_TEST_OUTPUT)
	else
		" https://github.com/junegunn/vader.vim ... vader#print_stderr
		for line in s:errors
		  verbose echon line."\n"
		endfor
	endif
endfunction "}}}

function s:has_overrides() "{{{
	return len(filter(copy(s:g), 'v:key !~# "^_"'))
endfunction "}}}

function s:reset_chain() "{{{
	if has_key(s:g, '_flattened')
		unlet s:g._flattened
	endif
endfunction "}}}

function s:clear_overrides() "{{{
	call filter(s:g, '!has_key(s:o, v:key)')
	call s:reset_chain()
endfunction "}}}

function s:runfail(test_func, ...) "{{{
	" ... => [exit hander][defer]
	let ec = 1000
	let Handler = a:0 ? a:1 : v:null
	let defer = a:0 == 2 ? a:2 : v:false
	try
		call a:test_func()
		let ec = 0
	catch /.*/
		let m = matchlist(v:exception, '^Vim\%((\a\+)\)\=:E\(\d\+\)')
		let ec = get(m, 1, 1001)
		call add(s:errors, s:_fmterrors(
					\ ec, printf('%s: %s', v:throwpoint, v:exception)
					\ ))
	finally
		if type(Handler) == 2
			try
				call Handler()
			catch /.*/
				call add(s:errors, v:exception)
				let ec = 1002
			endtry
		endif
		if !empty(s:errors) && !defer
			verbose echo join(s:errors, "\n") . "\n"
		endif
		if !empty(v:errors)
			let ec = len(v:errors)
			call s:_report()
		endif
		if ec && !defer
			cquit!
		endif
		call s:clear_overrides()
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
	call cursor(1, 1)
	normal! dG
	call append(0, a:src)
	normal! dG
	call assert_equal(a:src[-1], getline('$'))
	silent write
endfunction "}}}


" Utils (above) ---------------------------------------------------------------

call assert_notequal(s:g.runner, s:o.runner)
call assert_equal(1, s:has_overrides())
call s:clear_overrides()
call assert_false(s:has_overrides())
call s:runfail(function('assert_true', [v:true])) " No file-scope v:errors yet

function s:test_fail(ecode)
	throw 'Should be '. a:ecode
endfunction
call assert_equal(1001, s:runfail(
			\ funcref('s:test_fail', [1001]), v:null, v:true
			\ ))
let s:_soon = 1 == len(s:errors) && match(s:errors[0], 'Should be 1001') != -1
let s:errors = []
call s:runfail(function('assert_true', [s:_soon]))

call assert_equal(1002, s:runfail(
			\ function('acos', [-1]), funcref('s:test_fail', [1002]), v:true
			\ ))
let s:_soon = ['Should be 1002'] == s:errors
let s:errors = []
call s:runfail(function('assert_true', [s:_soon]))

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
call s:runfail(function('assert_true', [s:_soon]))
unlet s:_soon


" call ------------------------------------------------------------------------

function s:test_call() "{{{
	call assert_true(exists('*'. s:pfx .'call'))
	let Call = funcref(s:pfx . 'call')
	let overrideables = [
				\ 'extend_python_path', 'get_context', 'get_node_id',
				\ 'query_helper', 'runner', 'split'
				\ ]
	call assert_equal(overrideables, sort(keys(s:o)))
	let s:g['get_context'] = {-> a:000}
	let args = [1, 2, 3]
	call assert_equal(args, Call('get_context', args))
	" Same dict is reused unless reset
	let s:g['extend_python_path'] = {-> a:000}
	try
		" XXX if this is supposed to propagate, must always use in tandem?
		call assert_fails(Call('extend_python_path', args))
	catch /.*/
		call assert_exception('E118:') "Too many arguments
	endtry
	call assert_equal(args, Call('extend_python_path', args, 1)) " reset
	"
	let flat = s:g._flattened
	function s:g.get_node_id()
		return self
	endfunction
	call assert_notequal(flat, Call('get_node_id', [], 1)) " reset
	let flat = s:g._flattened
	call assert_equal(flat, Call('get_node_id', []))
	"
	function! g:pytest_pdb_break_overrides.runner(...) closure
		call assert_equal(overrideables, sort(keys(self)))
		return args == a:000
	endfunction
	call assert_true(Call('runner', args, 1))
endfunction "}}}

call s:runfail(funcref('s:test_call'))


" get_context -----------------------------------------------------------------

function s:test_get_context() "{{{
	call assert_false(s:s.exists('s:plugin'))
	call assert_false(s:s.exists('s:home'))
	call assert_false(s:s.exists('s:helper'))
	call assert_false(exists('b:pytest_pdb_break_context'))
	call s:o.get_context()
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
	call s:o.get_context()
	call assert_true(has_key(b:pytest_pdb_break_context, '/tmp/fakepython'))
	let after = map(['plugin', 'home', 'helper'], 's:s.get(v:val)')
	call assert_equal(before, after)
	unlet b:pytest_pdb_break_python_exe
	" Hack PATH (as done by $VIRTUAL_ENV/bin/activate)
	let origpath = $PATH
	let vbin = s:tempdir .'/.venv_base/bin'
	let vpy = vbin . '/python3'
	let vpt = vbin . '/pytest'
	let $PATH = vbin .':'. $PATH
	call assert_equal(vpt, exepath('pytest'))
	call s:o.get_context()
	call assert_true(has_key(b:pytest_pdb_break_context, vpy))
	let $PATH = origpath
endfunction "}}}

call s:pybuf('test_get_context')


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
			\ '    def inner():',
			\ '        return True',
			\ '    vartwo = True',
			\ '    assert vartwo',
			\ ]

let s:src_one_class = [
			\ 'class TestClass:',
			\ '    """',
			\ '    class Example:',
			\ '    """',
			\ '    def test_one(self):',
			\ '        varone = 1',
			\ '        assert varone',
			\ '',
			\ '    def test_two(self, request):',
			\ '        vartwo = 2',
			\ '        assert vartwo',
			\ ]

function s:test_get_node_id_two_funcs() "{{{
	" Setup
	let buf = bufname('%')
	call assert_true(buf =~# '^/')
	call s:write_src(s:src_two_funcs)
	" Baseline
	call cursor(1, 1)
	let pos = searchpos('varone')
	call assert_notequal([0, 0], pos)
	" vint: -ProhibitCommandRelyOnUser
	normal [m
	call assert_true(getline('.') =~# 'def test_first')
	" String
	call cursor(pos)
	let rv = s:o.get_node_id()
	call assert_equal(join([buf, 'test_first'], '::'), rv)
	" External
	let ext_pos = searchpos('# a comment')
	call assert_notequal([0, 0], ext_pos)
	let [line_num, column] = pos
	let rv = s:o.get_node_id(1, [0, line_num, column, 0])
	call assert_equal([buf, 'test_first'], rv)
	call assert_equal(ext_pos, getpos('.')[1:2])
	" List
	call cursor(pos)
	let rv = s:o.get_node_id(1)
	call assert_equal([buf, 'test_first'], rv)
	call assert_equal(pos, getpos('.')[1:2])
	" In def line
	call cursor(1, 1)
	call assert_true(search('test_first') > 0)
	let rv = s:o.get_node_id(1)
	call assert_equal([buf, 'test_first'], rv)
	" Last line
	call cursor(line('$'), 1)
	normal! $
	let rv = s:o.get_node_id(1)
	call assert_equal([buf, 'test_last'], rv)
	" No match
	call cursor(ext_pos)
	let [__, out] = s:capture(funcref(s:o.get_node_id, [1]))
	call assert_match('No test found', out, 'Got: '. string(__))
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
	" vint: -ProhibitCommandRelyOnUser
	normal [m
	call assert_true(getline('.') =~# 'def test_one')
	" String
	call cursor(pos)
	let rv = s:o.get_node_id()
	call assert_equal(buf .'::TestClass::test_one', rv)
	" With signature
	call setline(1, 'class TestClass(object):')
	call assert_equal(pos, getpos('.')[1:2])
	let rv = s:o.get_node_id()
	call assert_equal(buf .'::TestClass::test_one', rv)
	" Between
	call cursor(1, 1)
	let pos = searchpos('^$')
	call assert_notequal([0, 0], pos)
	let [__, out] = s:capture(funcref(s:o.get_node_id, []))
	call assert_match('No test found', out)
	" Last line
	call cursor(line('$'), 1)
	let rv = s:o.get_node_id()
	call assert_equal(buf .'::TestClass::test_two', rv)
	" Indentation level
	" FIXME bad example (implies there's some fixture named 'self')
	let unlikely = ['', 'def test_arg(self):', '    return self']
	call s:write_src(s:src_one_class + unlikely)
	call cursor(line('$'), 1)
	let rv = s:o.get_node_id()
	call assert_equal(buf .'::test_arg', rv)
endfunction "}}}

call s:pybuf('test_get_node_id_two_funcs')
call s:pybuf('test_get_node_id_one_class')


" query_helper ----------------------------------------------------------------

function s:test_query_helper() "{{{
	" ERROR - no pytest (unrealistic since get_context() uses shebang)
	call assert_true(filereadable(s:s.get('helper')))
	let ctx = {'exe': s:tempdir . '/.venv_bare/bin/python'}
	call assert_equal(ctx.exe, exepath(ctx.exe), 'exepath mismatch')
	let [__, out] = s:capture(funcref(s:o.query_helper, [ctx]))
	call assert_match('Traceback.*ModuleNotFoundError', out)
	call assert_equal({'exe': s:tempdir . '/.venv_bare/bin/python'}, ctx)
	" Has pytest
	let ctx = {'exe': s:tempdir . '/.venv_base/bin/python'}
	call s:o.query_helper(ctx)
	call assert_false(ctx.registered)
	call assert_equal(getcwd(), ctx.rootdir)
	call assert_equal(s:tempdir . '/.venv_base/bin/python', ctx.exe)
	" ERROR - Bad option
	let ctx = {'exe': s:tempdir . '/.venv_base/bin/python'}
	let [__, out] = s:capture(funcref(s:o.query_helper, [ctx, '--fake']))
	call assert_match('unrecognized arguments', out)
	call assert_equal({'exe': s:tempdir . '/.venv_base/bin/python'}, ctx)
	" Node-id - nonexistent/unsaved file
	call assert_false(filereadable(bufname('%')))
	let ctx = {'exe': s:tempdir . '/.venv_base/bin/python'}
	call s:o.query_helper(ctx, bufname('%'))
	call assert_false(ctx.registered)
	call assert_equal(getcwd(), ctx.rootdir)
	" Node-id - nonexistent/unsaved file and unknown func
	let ctx = {'exe': s:tempdir . '/.venv_base/bin/python'}
	call s:o.query_helper(ctx, bufname('%') .'::test_fake')
	call assert_false(ctx.registered)
	call assert_equal(getcwd(), ctx.rootdir)
	" Node-id - real file, unknown func
	call s:write_src(s:src_two_funcs)
	let ctx = {'exe': s:tempdir . '/.venv_base/bin/python'}
	call s:o.query_helper(ctx, bufname('%') .'::test_fake')
	call assert_false(ctx.registered)
	call assert_equal(getcwd(), ctx.rootdir)
	" Node-id - real file, real func
	let ctx = {'exe': s:tempdir . '/.venv_base/bin/python'}
	call s:o.query_helper(ctx, bufname('%') .'::test_first')
	call assert_false(ctx.registered)
	call assert_equal(getcwd(), ctx.rootdir)
	" Alt rootdir (empty, not ancestor of CWD or node-id path)
	let ctx = {'exe': s:tempdir . '/.venv_base/bin/python'}
	let altroot = s:temphome .'/test_get_context'
	call s:o.query_helper(ctx, '--rootdir='. altroot, bufname('%'))
	call assert_false(ctx.registered)
	call assert_equal(altroot, ctx.rootdir)
	" Has plugin
	let ctx = {'exe': s:tempdir . '/.venv_self/bin/python'}
	call s:o.query_helper(ctx, bufname('%') .'::test_fake')
	call assert_true(ctx.registered)
	call assert_equal(getcwd(), ctx.rootdir)
	" With ini
	call writefile(['[pytest]', 'addopts = -v'], 'pytest.ini')
	let ctx = {'exe': s:tempdir . '/.venv_self/bin/python'}
	call s:o.query_helper(ctx, bufname('%') .'::test_fake')
	call assert_true(ctx.registered)
	call assert_equal(getcwd(), ctx.rootdir)
endfunction "}}}

call s:pybuf('test_query_helper')


" extend_python_path ----------------------------------------------------------

function s:test_extend_python_path() "{{{
	try
		unlet $PYTHONPATH
	catch /^Vim\%((\a\+)\)\=:E488/
		let $PYTHONPATH = ''
	endtry
	call assert_true(empty($PYTHONPATH))
	"
	let ctx = {'PP': '/tmp/fake'}
	call assert_equal(ctx.PP, s:o.extend_python_path(ctx))
	unlet ctx.PP
	let home = s:s.get('home')
	call assert_true(filereadable(home . '/tox.ini'))
	call assert_equal(home, s:o.extend_python_path(ctx))
	call assert_equal(ctx.PP, home)
	unlet ctx.PP
	"
	let first = s:temphome . '/first'
	let $PYTHONPATH = first
	call assert_equal(home .':'. first, s:o.extend_python_path(ctx))
	call assert_equal(home .':'. first, ctx.PP)
	" No uniq-like filtering
	let $PYTHONPATH = ctx.PP
	unlet ctx.PP
	let expected = join([home, home, first], ':')
	call assert_equal(expected, s:o.extend_python_path(ctx))
	call assert_equal(expected, ctx.PP)
	try
		unlet $PYTHONPATH
	catch /^Vim\%((\a\+)\)\=:E488/
		let $PYTHONPATH = ''
	endtry
endfunction "}}}

call s:runfail(funcref('s:test_extend_python_path'))


" runner ----------------------------------------------------------------------

function s:test_runner() "{{{
	" Mock split, get_node_id
	let thisbuf = bufname('%')
	let dirname = fnamemodify(bufname('%'), ':h')
	let s:g.split = {-> a:000}
	let s:g.get_node_id = {-> [thisbuf, 'test_first']}
	let b:pytest_pdb_break_python_exe = '/tmp/fakepython'
	let ctx = s:o.get_context()
	" Inject plugin
	let ctx.registered = v:false
	let ctx.rootdir = dirname
	let rv = s:o.runner('⁉')
	let plhome = s:s.get('home')
	if has('nvim')
		call assert_equal([
					\ 'env', 'PYTHONPATH='. plhome,
					\ '/tmp/fakepython', '-mpytest'
					\ ], ctx.last.cmd)
	else
		call assert_equal(['/tmp/fakepython', '-mpytest'], ctx.last.cmd)
	endif
	call assert_equal(['⁉'], ctx.last.uopts)
	call assert_equal([
				\ '-p', 'pytest_pdb_break',
				\ '--break='. thisbuf .':1'
				\ ], ctx.last.opts)
	call assert_equal([thisbuf, 'test_first'], ctx.last.node_id)
	let expect_jobd = {'cwd': dirname }
	if !has('nvim')
		let expect_jobd.env = {'PYTHONPATH': plhome}
	endif
	call assert_equal([
				\ ctx.last.cmd
				\ + ctx.last.uopts
				\ + ctx.last.opts
				\ + [thisbuf .'::test_first'],
				\ expect_jobd
				\ ], rv)
	" Plugin present
	let ctx.registered = v:true
	let ctx.rootdir = dirname
	let rv = s:o.runner('⁉')
	call assert_equal([[
				\ '/tmp/fakepython', '-mpytest',
				\ '⁉', '--break='. thisbuf .':1',
				\ thisbuf .'::test_first'
				\ ], {'cwd': dirname}], rv)
	" Change in args triggers call to query_helper (mocked)
	function! s:g.query_helper(...)
		echon 'query_helper ran'
	endfunction
	call s:reset_chain()
	let [rv, out] = s:capture(funcref(s:o.runner, []))
	call assert_match('query_helper ran', out)
	call assert_equal([], ctx.last.uopts)
	let expect_cmdl = [
				\ '/tmp/fakepython', '-mpytest',
				\ '--break='. thisbuf .':1',
				\ thisbuf.'::test_first'
				\ ]
	let expect_jobd = {'cwd': dirname}
	call assert_equal([expect_cmdl, expect_jobd], rv)
	" As does absence of registration check
	unlet ctx.registered
	let s:g.query_helper = {-> extend(ctx, {'registered': v:true})}
	call s:reset_chain()
	let rv = s:o.runner()
	call assert_true(exists('ctx.registered'))
	call assert_equal([expect_cmdl, expect_jobd], rv)
endfunction "}}}

call s:pybuf('test_runner')


" split -----------------------------------------------------------------------

function s:test_split() "{{{
	let exe = s:tempdir . '/.venv_base/bin/python'
	let buf = bufname('%')
	let jobd = {'commands': ['import sys', 'sys.path[1]'], 'output': []}
	if has('nvim')
		let cmdl = ['env', 'PYTHONPATH=/tmp/fake', exe, '-i']
		function! s:on_stdout(id, data, event) dict
			let str = join(a:data, '')
			call add(self.output, str)
			let comms = self.commands
			if str =~# '>>> $'
				if !empty(comms)
					call chansend(a:id, [comms[0], ''])
					call remove(comms, 0)
				else
					call chansend(a:id, ['', ''])
				endif
			endif
		endfunction
		let jobd.on_stdout = function('s:on_stdout')
	else
		let cmdl = [exe, '-i']
		function! s:on_out(chan, msg) dict
			call add(self.output, a:msg)
			if a:msg =~# '>>> $'
				if !empty(self.commands)
					call ch_sendraw(a:chan, self.commands[0] . "\n")
					call remove(self.commands, 0)
				else
					call ch_sendraw(a:chan, "\x4")
				endif
			endif
		endfunction
		let _jobd = jobd
		let jobd = {}
		let jobd.env = {'PYTHONPATH': '/tmp/fake'}
		let jobd.out_cb = funcref('s:on_out', _jobd)
		call ch_logfile(expand('%:p:h') .'/vim8-channel.log', 'a')
	endif
	call s:o.split(cmdl, jobd)
	call assert_true(has_key(jobd, 'vertical'))
	call assert_true(has_key(jobd, 'job'))
	call assert_notequal(buf, bufname('%'))
	if has('nvim')
		call assert_equal([0], jobwait([jobd.job], 5000))
		execute 'bdelete! term'
		let outlines = split(join(jobd.output, ''), "\r\\|\n")
		call assert_match('/tmp/fake', join(jobd.output, ''))
	else
		let bn = ch_getbufnr(jobd.job, 'out')
		let waited = 0
		while (ch_status(jobd.job) !=? 'closed'
					\ || job_status(jobd.job) !=? 'dead')
					\ && waited < 2000
			call term_wait(bn)
			let waited += 10
		endwhile
		call ch_log('waited: '. waited .'ms')
		bdelete!
		let outlines = split(join(_jobd.output, ''), "\r\n")
		call assert_match('/tmp/fake', join(_jobd.output, ''))
	endif
	call writefile(outlines, 'term.log')
endfunction "}}}

call s:pybuf('test_split')

" -----------------------------------------------------------------------------
quitall!
