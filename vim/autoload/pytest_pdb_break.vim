scriptencoding utf-8

let s:file = expand('<sfile>')
let s:initialized = v:false

function! pytest_pdb_break#run(...) abort
  return s:call('runner', a:000)
endfunction

function! s:call(name, args, ...) abort
  " ... => dict
  if a:0
    if type(a:1) != v:t_dict
      throw 'Wrong type for a:1'
    endif
    let d = a:1
  else
    let d = filter(copy(g:pytest_pdb_break_overrides), 'v:key !~# "^_"')
    call extend(d, s:defuncs, 'keep')
  endif
  return call(d[a:name], a:args)
endfunction

function! s:_get_pytest_exe() abort
  if exists('b:pytest_pdb_break_pytest_exe')
    let exe = b:pytest_pdb_break_pytest_exe
  else
    let exe = exepath('pytest')
  endif
  if !executable(exe)
    throw 'Cannot find pytest'
  endif
  return exe
endfunction

function! s:_get_interpreter(vars) abort
  try
    if !has_key(a:vars, 'pytest_exe')
      let a:vars['pytest_exe'] =  s:_get_pytest_exe()
    endif
    let shebang = readfile(a:vars.pytest_exe, '', 1)[0]
    let exe = substitute(shebang, '^#!', '', '')
  catch /.*/
    let exe = ''
  endtry
  if executable(exe)
    return exe
  endif
  " Plain python will fail if not python3
  let backups = exists('g:python3_host_prog') ? [g:python3_host_prog] : []
  let backups += ['python3', 'python']
  for exe in backups
    if executable(exe)
      return exepath(exe)
    endif
  endfor
  throw 'Could not find a python executable'
endfunction

function! s:init(vars) abort
  " vars => dict with optional items:
  "   'interpreter': path to python interpreter
  "   'pytest_exe': path to pytest exe
  "
  call assert_equal(&filetype, 'python')
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
    let s:isolib = tempname()
    if !has_key(a:vars, 'interpreter')
      let a:vars['interpreter'] = s:_get_interpreter(a:vars)
    endif
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
  let vars = {}
  if !s:initialized
    call self.init(vars)
  endif
  if !exists('b:pytest_pdb_break_context')
    let b:pytest_pdb_break_context = {}
  endif
  if !exists('vars.interpreter') " implies no pytest_exe
    let vars.interpreter = s:_get_interpreter(vars)
  endif
  let exe = vars.pytest_exe
  if !has_key(b:pytest_pdb_break_context, exe)
    let b:pytest_pdb_break_context[exe] = vars
  endif
  return b:pytest_pdb_break_context[exe]
endfunction

function! s:_search_back(pat, test) abort
  let begd = indent('.')
  let begl = line('.')
  while 1
    if !search(a:pat, 'Wb')
      return 0
    endif
    let sid = synID(line('.'), col('.'), 1)
    if synIDattr(sid, 'name') !~? 'comment\|string' && a:test(begl, begd)
      break
    endif
  endwhile
  return line('.')
endfunction

function! s:_get_node_id_parts(...) abort
  " ... => [want list][start pos]
  let spos = getcurpos()
  try
    if a:0 == 2
      call setpos('.', a:2)
    endif
    normal! $
    let gr = []
    if s:_search_back('\v^\s*(def|async def)>',
          \ {l, d -> indent('.') < d || line('.') == l})
      let pat = '^\s*\(def\|async def\)\s\+\(test_\w\+\)(\(.*\)).*$'
      let gr = matchlist(getline('.'), pat)
    endif
    if empty(gr) || empty(gr[2])
      echohl WarningMsg | echo 'No test found!' | echohl Normal
      echo printf(' beg(%d): %s', spos[1], shellescape(getline(spos[1])))
      echo printf(' end(%d): %s', line('.'), shellescape(getline('.')))
      echo ' match: ' . string(gr)
      throw 'Search failed'
    endif
    let nodeid = [gr[2]]
    if gr[3] =~# '^self.*' &&
          \ s:_search_back('\_^\s*class\s', {_, d -> indent('.') < d})
      let cgr = matchlist(getline('.'), '^\s*class\s\(\w\+\).*:.*')
      call add(nodeid, cgr[1])
    endif
  finally
    call setpos('.', spos)
  endtry
  call add(nodeid, expand('%:p'))
  return a:0 && a:1 ? reverse(nodeid) : join(reverse(nodeid), '::')
endfunction

function! s:_print_error(msg, kwargs) abort
    echohl WarningMsg | echo a:msg | echohl None
    for [k, V] in items(a:kwargs)
      echo k .': '. (type(V) != v:t_string ? string(V) : V)
    endfor
endfunction

function! s:_check_json(ctx, cmd, ...) abort
  let context = a:ctx
  let cmdline = [context.interpreter, s:helper, '--json', a:cmd, '--'] + a:000
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
    throw v:exception
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
  let locs = call('s:_check_json', [ctx, 'get_collected'] + ctx.args)
  let _s = self
  "
  function! ctx.ll_callback(obj) closure abort
    let self.new_item = [a:obj.file] + split(a:obj.nodeid, '::')[1:]
    return call(_s.runner, ctx.args)
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
      silent let nid = s:_get_node_id_parts(1)
    catch /^Search failed/
      try
        let nid = self.prompt_for_item()
      catch /^Awaitathon/
        return ''
      catch /.*/
        silent! unlet! ctx.new_item " docs say unlet! form suppresses, but?
        throw substitute(v:exception,'^Vim\(.\+\)$', '\1', '')
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
  if !exists('self.session')
    let self.session = self.get_context()
  endif
  let ctx = self.session
  let ctx.args = a:000
  let nid = self.get_node_id()
  if empty(nid)
    return 0
  endif
  let cmd = [ctx.pytest_exe]
  let opts = []
  let jd = {}
  let preenv = self.extend_python_path()
  if has('nvim')
    let cmd = ['env', printf('PYTHONPATH=%s', preenv)] + cmd
  else
    let jd.env = {'PYTHONPATH': preenv}
  endif
  call add(opts,  printf('--break=%s:%s', expand('%:p'), line('.')))
  let ctx.last = {
        \ 'cmd': cmd, 'uopts': ctx.args, 'opts': opts,
        \ 'node_id': nid, 'jd': jd
        \ }
  return self.split(cmd + ctx.args + opts + [nid], jd)
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


let s:defuncs = {
      \ 'init': funcref('s:init'),
      \ 'get_context': funcref('s:get_context'),
      \ 'extend_python_path': funcref('s:extend_python_path'),
      \ 'prompt_for_item': funcref('s:prompt_for_item'),
      \ 'get_node_id': funcref('s:get_node_id'),
      \ 'runner': funcref('s:runner'),
      \ 'split': funcref('s:split')
      \ }

if exists('g:pytest_pdb_break_testing')
  let g:pytest_pdb_break_testing.s = {
        \ 'exists': {n -> exists('s:'. n)},
        \ 'get': {n -> eval('s:'. n)},
        \ 'assign': {n, v -> execute('let s:'. n .' = '. v)},
        \ 'forget': {n -> execute('unlet s:'. n)}
        \ }
  let g:pytest_pdb_break_testing.o = copy(s:defuncs)
endif
