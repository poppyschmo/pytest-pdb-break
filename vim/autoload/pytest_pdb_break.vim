scriptencoding utf-8

let s:file = expand('<sfile>')
let s:initialized = v:false

function! pytest_pdb_break#run(...) abort
  try
    return call(pytest_pdb_break#new().runner, a:000)
  catch /^Vim\%((\a\+)\)\=:E171/
    " Normally redundant; topmost msg shows callers for whole stack
  endtry
endfunction

function! pytest_pdb_break#new(...) abort
  " Return a new call space with .session data member
  " ... => optional dict containing one-off overrides
  let d = extend(copy(g:pytest_pdb_break_overrides), s:dfuncs, 'keep')
  if a:0
    call extend(d, a:1)
  endif
  if !exists('d.session')
    let d.session = call(d.get_context, [])
  endif
  return d
endfunction

function! s:_get_pytest_exe() abort
  if exists('b:pytest_pdb_break_pytest_exe')
    let exe = b:pytest_pdb_break_pytest_exe
  else
    let exe = exepath('pytest')
  endif
  if !executable(exe)
    throw 'Could not find pytest'
  endif
  return exe
endfunction

function! s:_get_interpreter(pytest_exe) abort
  let lines = []
  try
    let lines = readfile(a:pytest_exe)
    let exe = substitute(lines[0], '^#!', '', '')
  catch /.*/
    let exe = ''
  endtry
  if executable(exe)
    return exe
  endif
  if !empty(lines) && join(lines) =~# 'PYENV'
    let cmdline = ['pyenv', 'which', 'pytest']
    if !has('nvim')
      let cmdline = join(cmdline)
    endif
    let pytest_exe = matchstr(system(cmdline), '.\+\w\ze[[:space:]]*$')
    if pytest_exe != a:pytest_exe && executable(pytest_exe)
      return s:_get_interpreter(pytest_exe)
    endif
  endif
  " Don't bother trying other pythons in PATH, even host_prog
  throw 'Could not find a python executable'
endfunction

function! s:_init(vars) abort
  " Without g:pytest_pdb_break_alt_lib, vars dict must contain:
  "   'pytest_exe': path to pytest exe
  " And will set:
  "   'interpreter': path to python interpreter
  "
  let path = fnamemodify(s:file, ':h:p') . ';'
  let plugin = findfile('pytest_pdb_break.py', path)
  if empty(plugin)
    throw 'Could not find plugin root above entry in &rtp'
  endif
  let s:plugin = fnamemodify(plugin, ':p') " covers the rare './plugin'
  let s:home = fnamemodify(s:plugin, ':h:p') " no trailing/
  let s:helper = simplify(s:home . '/helpers/main.py')
  if exists('g:pytest_pdb_break_alt_lib')
    " Glob still passes if pat contains double / (no need to simplify)
    let alt_lib = g:pytest_pdb_break_alt_lib
    if !isdirectory(alt_lib) ||
          \ empty(glob(alt_lib .'/pytest_pdb_break*-info'))
      throw 'Invalid alt lib: ' . alt_lib
    endif
    let s:isolib = alt_lib
  else
    let a:vars.interpreter = s:_get_interpreter(a:vars.pytest_exe)
    let s:isolib = tempname()
    let cmdline = [a:vars.interpreter, s:helper, 'install_plugin', s:isolib]
    if !has('nvim')
      let cmdline = join(cmdline)
    endif
    call system(cmdline)
    if !isdirectory(s:isolib)
      throw 'Problem calling '. s:helper
    endif
  endif
  let s:initialized = v:true
endfunction

function! s:get_context() dict abort
  " Save session/env info in buffer-local dict, keyed by pytest exe
  let pytest_exe = s:_get_pytest_exe()
  let vars = {'pytest_exe': pytest_exe}
  if !s:initialized
    call s:_init(vars)
  endif
  if !exists('b:pytest_pdb_break_context')
    let b:pytest_pdb_break_context = {}
  endif
  if !has_key(b:pytest_pdb_break_context, pytest_exe)
    let b:pytest_pdb_break_context[pytest_exe] = vars
  endif
  return b:pytest_pdb_break_context[pytest_exe]
endfunction

function! s:_search_back(pat, test) abort
  let lastd = indent('.')
  let found = []
  while search(a:pat, 'Wb')
    let sid = synID(line('.'), col('.'), 1)
    if synIDattr(sid, 'name') =~? 'comment\|string'
      continue
    endif
    if indent('.') >= lastd
      continue
    endif
    let lastd = indent('.')
    if a:test()
      let found = getpos('.')
      break
    endif
  endwhile
  if empty(found)
    return 0
  endif
  call setpos('.', found)
  return 1
endfunction

function! s:_get_node_id_parts(...) abort
  " ... => [start pos]
  let spos = getcurpos()
  try
    if a:0
      call setpos('.', a:1)
    endif
    normal! $
    let gr = []
    if s:_search_back('\v^\s*(def|async def)>',
          \ {-> getline('.') =~ 'def\s\+test_'})
      let pat = '^\s*\%(async\s\)\?def\s\+\(test_\w\+\)(\(.*\)).*$'
      let gr = matchlist(getline('.'), pat)
    endif
    if empty(gr) || empty(gr[1])
      echohl WarningMsg | echo 'No test found!' | echohl Normal
      echo printf(' beg(%d): %s', spos[1], shellescape(getline(spos[1])))
      echo printf(' end(%d): %s', line('.'), shellescape(getline('.')))
      echo ' match: ' . string(gr)
      throw 'Search failed'
    endif
    let nodeid = [gr[1]]
    " FIXME don't hard-code Test* as name (can be changed in ini file)
    if gr[2] =~# '^self.*' &&
          \ s:_search_back('\_^\s*class\s', {-> getline('.') =~ 'class\s\+Test'})
      let cgr = matchlist(getline('.'), '^\s*class\s\(\w\+\).*:.*')
      call add(nodeid, cgr[1])
    endif
  finally
    call setpos('.', spos)
  endtry
  call add(nodeid, expand('%:p'))
  return reverse(nodeid)
endfunction

function! s:_print_error(msg, kwargs) abort
    echohl WarningMsg | echo a:msg | echohl None
    for [k, V] in items(a:kwargs)
      echo k .': '. (type(V) != v:t_string ? string(V) : V)
    endfor
endfunction

function! s:_check_json(ctx, cmd, ...) abort
  if !exists('a:ctx.interpreter')
    let a:ctx.interpreter = s:_get_interpreter(a:ctx.pytest_exe)
  endif
  let cmdline = [a:ctx.interpreter, s:helper, '--json', a:cmd, '--'] + a:000
  if !has('nvim')
    let cmdline = join(cmdline)
  endif
  try
    let result = system(cmdline)
    try
      let decoded = json_decode(result)
    catch /^Vim\%((\a\+)\)\=:E474/
      throw result
    endtry
    if type(decoded) == v:t_dict && has_key(decoded, 'error')
      let result = join(decoded.traceback, '')
      throw result
    endif
  catch /.*/
    let md = {'path': s:helper, 'cmdline': cmdline,}
    " Truncation often lops off final exc (most recent call last)
    if exists('result') && v:exception =~# 'Traceback'
      if stridx(v:exception, result) == -1
        let md.exc = matchstr(v:exception, '.*\zeTraceback') . result
      else
        let md.exc = result
      endif
    else
      let md.exc = v:exception
    endif
    call s:_print_error('Problem calling helper', md)
    echoerr 'HelperError'
  endtry
  return decoded
endfunction

function! s:_present_loclist(context, locs) abort
  let ctx = a:context
  let curwin = winnr()
  let curview = winsaveview()
  let orig = getloclist(curwin, {'all': 1})
  let act = empty(orig) ? 'f' : 'r'
  let F = {k, v -> {'filename': v.file, 'lnum': v.lnum, 'text': v.name}}
  function! s:_select_and_remove(want) closure abort
    let dex = line('.') - 1
    nunmap <buffer> <cr>
    nunmap <buffer> q
    call timer_stop(ctx.ll_timer)
    silent! call setloclist(curwin, [], act, orig)
    if act ==# 'r'  " remove prepended colon (hack?)
      silent! call setloclist(curwin, [], 'r', {'title': orig.title})
    endif
    lclose
    call winrestview(curview)
    if a:want
      call ctx.ll_callback(a:locs[dex])
    endif
  endfunction
  function! s:_p(...) abort
    echohl ModeMsg | echo '<CR> selects, q aborts' | echohl None
  endfunction
  try
    let locs = map(copy(a:locs), F)
    " Would prefer rootdir over bufname, but we're flying blind here
    let title = printf('Pytest[items]: %s (%d)', bufname('%'), len(locs))
    let what = {'items': locs, 'title': title, 'context': ctx}
    call setloclist(curwin, [], 'r', what)  " clobber
    lopen
    noremap <buffer><nowait><silent> <cr> :call <SID>_select_and_remove(1)<cr>
    noremap <buffer><nowait><silent> q :call <SID>_select_and_remove(0)<cr>
    let ctx.ll_timer = timer_start(100, funcref('<SID>_p'))
  catch /.*/
    silent! call setloclist(curwin, [], act, orig)  " restore
    if &buftype ==# 'quickfix'
      silent! nunmap <buffer> <cr>
      silent! nunmap <buffer> q
      silent! lclose
    endif
    echoerr v:exception
  endtry
endfunction

function! s:_defer_to_denite(context, locs) abort
  let d = {'_pytest_item_locations': a:locs}
  " Would be nice to use a Kind.action__* method, but can't figure out how to
  " access funcrefs from a py module.
  call denite#custom#action('pytest_item', 'execute',
        \ {c -> a:context.ll_callback(c['targets'][0]['action__command'])})
  call denite#start([{'name': 'pytest_items', 'args': []}], d)
endfunction

function! s:prompt_for_item() dict abort
  let ctx = self.session
  let locs = call('s:_check_json', [ctx, 'get_collected'] + ctx.opts)
  let _s = self
  "
  function! ctx.ll_callback(obj) closure abort
    let self.new_item = [a:obj.file] + split(a:obj.nodeid, '::')[1:]
    return call(_s.runner, ctx.session_opts)
  endfunction
  "
  if exists('g:loaded_denite')
    call s:_defer_to_denite(ctx, locs)
  else
    call s:_present_loclist(ctx, locs)
  endif
  throw 'Awaitathon'
endfunction

function! s:get_node_id() dict abort
  let ctx = self.session
  let nid = get(ctx, 'new_item', [])
  if empty(nid)
    try
      silent let nid = s:_get_node_id_parts()
    catch /^Search failed/
      try
        let nid = self.prompt_for_item()
      catch /^Awaitathon/
        return ''
      catch /.*/
        silent! unlet! ctx.new_item " docs say unlet! form suppresses, but?
        echoerr v:exception
      endtry
    endtry
  else
    unlet ctx.new_item
  endif
  return join(nid, '::')
endfunction

function! s:extend_python_path() dict abort
  " Stash modified copy of PYTHONPATH, should be unset if unneeded
  let ctx = self.session
  if has_key(ctx, 'PP')
    return ctx.PP
  endif
  let val = empty($PYTHONPATH) ? [] : split($PYTHONPATH, ':')
  let val = filter(val, 'v:val != s:isolib')
  call insert(val, s:isolib)
  let ctx.PP = join(val, ':')
  return ctx.PP
endfunction

function! s:runner(...) dict abort
  let ctx = self.session
  let ctx.opts = g:pytest_pdb_break_extra_opts +
        \ get(b:, 'pytest_pdb_break_extra_opts', [])
  let ctx.session_opts = a:000  " needed by query helper
  let nid = self.get_node_id()
  if empty(nid)
    return 0
  endif
  let cmd = [ctx.pytest_exe]
  let jd = {}
  let preenv = self.extend_python_path()
  if has('nvim')
    let cmd = ['env', printf('PYTHONPATH=%s', preenv)] + cmd
  else
    let jd.env = {'PYTHONPATH': preenv} " uses curent env as base, updates
  endif
  let plugopts = g:pytest_pdb_break_defaults +
        \ [printf('--break=%s:%s', expand('%:p'), line('.'))]
  let cmdline = cmd + ctx.opts + plugopts + ctx.session_opts + [nid]
  let ctx.jobd = jd
  return self.split(cmdline, jd)
endfunction

function! s:split(cmdline, jobd) abort
  if !has_key(a:jobd, 'vertical')
    if exists('b:pytest_pdb_break_vertical')
      let a:jobd.vertical = b:pytest_pdb_break_vertical
    else
      let wn = winnr()
      let a:jobd.vertical = (winwidth(wn) + 0.0) / winheight(wn) > 2.5
    endif
  endif
  if has('nvim')
    execute a:jobd.vertical ? 'vnew' : 'new'
    let a:jobd.job = termopen(a:cmdline, a:jobd)
    startinsert
  else
    let a:jobd.job = term_getjob(term_start(a:cmdline, a:jobd))
  endif
endfunction


let s:dfuncs = {
      \ 'get_context': funcref('s:get_context'),
      \ 'extend_python_path': funcref('s:extend_python_path'),
      \ 'prompt_for_item': funcref('s:prompt_for_item'),
      \ 'get_node_id': funcref('s:get_node_id'),
      \ 'runner': funcref('s:runner'),
      \ 'split': funcref('s:split')
      \ }

if exists('g:pytest_pdb_break_testing')
  function! s:_gen_prefix() abort
    " Can also use get() on func with 'name'
    return matchstr(expand('<sfile>'), '\zs<SNR>\d\+_\ze')
  endfunction
  let g:pytest_pdb_break_testing.s = {
        \ 'prefix': {-> s:_gen_prefix()},
        \ 'exists': {n -> exists('s:'. n)},
        \ 'get': {n -> eval('s:'. n)},
        \ 'assign': {n, v -> execute('let s:'. n .' = '. v)},
        \ 'forget': {n -> execute('unlet s:'. n)}
        \ }
  let g:pytest_pdb_break_testing.o = copy(s:dfuncs)
endif
