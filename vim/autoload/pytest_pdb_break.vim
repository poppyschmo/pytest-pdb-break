scriptencoding utf-8

let s:file = expand('<sfile>')
let s:initialized = v:false

function! pytest_pdb_break#run(...) abort
  return s:call('runner', a:000, 1)
endfunction

function! s:call(name, args, ...) abort
  " ... => [clear]
  let d = a:0 && a:1 ?
        \ {} : get(g:pytest_pdb_break_overrides, '_flattened', {})
  if empty(d)
    let d = filter(copy(g:pytest_pdb_break_overrides), 'v:key !~# "^_"')
    call extend(d, s:defuncs, 'keep')
    let g:pytest_pdb_break_overrides._flattened = d
  endif
  return call(d[a:name], a:args, d)
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

function! s:_get_interpreter() abort
  try
    let pytest_exe = s:_get_pytest_exe()
    let shebang = readfile(pytest_exe, '', 1)[0]
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

function! s:init() abort
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
    let cmdline = [s:_get_interpreter(), s:helper, 'install_plugin', s:isolib]
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

function! s:get_context() abort
  " Save env info in buffer-local dict, keyed by pytest exe
  " XXX the context-dict thing may have made sense under the old 'rootdir'
  " setup but now seems needlessly complicated; consider ditching
  if !s:initialized
    call s:call('init', [])
  endif
  if !exists('b:pytest_pdb_break_context')
    let b:pytest_pdb_break_context = {}
  endif
  let exe = s:_get_pytest_exe()
  if !has_key(b:pytest_pdb_break_context, exe)
    let b:pytest_pdb_break_context[exe] = {'exe': exe}
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
  let cmdline = [context.exe, s:helper, '--json', a:cmd, '--'] + a:000
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

function! s:_populate_loclist(context, locs) abort
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

function! s:prompt_for_item(ctx, ...) abort
  " TODO make denite source if installed
  let locs = call('s:_check_json', [a:ctx, 'get_collected'] + a:000)
  function! a:ctx.ll_callback(obj) abort closure
    let self.new_item = [a:obj.file] + split(a:obj.nodeid, '::')[1:]
    return s:call('runner', a:000)
  endfunction
  call s:_populate_loclist(a:ctx, locs)
  throw 'Awaitathon'
endfunction

function! s:get_node_id(ctx, ...) abort
  let nid = get(a:ctx, 'new_item', [])
  if empty(nid)
    try
      silent let nid = s:_get_node_id_parts(1)
    catch /^Search failed/
      try
        let nid = s:call('prompt_for_item', [a:ctx] + a:000)
      catch /^Awaitathon/
        return []
      catch /.*/
        silent! unlet! a:ctx.new_item " docs say unlet! form suppresses, but?
        throw substitute(v:exception,'^Vim\(.\+\)$', '\1', '')
      endtry
    endtry
  else
    unlet a:ctx.new_item
  endif
  return join(nid, '::')
endfunction

function! s:extend_python_path(ctx) abort
  " Stash modified copy of PYTHONPATH, should be unset if unneeded
  let ctx = a:ctx
  if has_key(ctx, 'PP')
    return ctx.PP
  endif
  let val = empty($PYTHONPATH) ? [] : split($PYTHONPATH, ':')
  let val = filter(val, 'v:val != s:isolib')
  call insert(val, s:isolib)
  let ctx.PP = join(val, ':')
  return ctx.PP
endfunction

function! s:runner(...) abort
  let ctx = s:call('get_context', [])
  let nid = s:call('get_node_id', [ctx] + a:000)
  if empty(nid)
    return 0
  endif
  let cmd = [ctx.exe]
  let opts = []
  let jd = {}
  let preenv = s:call('extend_python_path', [ctx])
  if has('nvim')
    let cmd = ['env', printf('PYTHONPATH=%s', preenv)] + cmd
  else
    let jd.env = {'PYTHONPATH': preenv}
  endif
  call add(opts,  printf('--break=%s:%s', expand('%:p'), line('.')))
  let ctx.last = {
        \ 'cmd': cmd, 'uopts': a:000, 'opts': opts,
        \ 'node_id': nid, 'jd': jd
        \ }
  return s:call('split', [cmd + a:000 + opts + [nid], jd])
endfunction

function! s:split(cmdl, jobd) abort
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
    let a:jobd.job = termopen(a:cmdl, a:jobd)
    startinsert
  else
    let a:jobd.job = term_getjob(term_start(a:cmdl, a:jobd))
  endif
endfunction


let s:defuncs = {
      \ 'get_context': funcref('s:get_context'),
      \ 'init': funcref('s:init'),
      \ 'extend_python_path': funcref('s:extend_python_path'),
      \ 'prompt_for_item': funcref('s:prompt_for_item'),
      \ 'get_node_id': funcref('s:get_node_id'),
      \ 'runner': funcref('s:runner'),
      \ 'split': funcref('s:split')
      \ }

if exists('g:pytest_pdb_break_testing')
  let g:pytest_pdb_break_testing.i = {
        \ '_get_node_id_parts': funcref('s:_get_node_id_parts'),
        \ }
  let g:pytest_pdb_break_testing.s = {
        \ 'exists': {n -> exists('s:'. n)},
        \ 'get': {n -> eval('s:'. n)},
        \ 'assign': {n, v -> execute('let s:'. n .' = '. v)},
        \ 'forget': {n -> execute('unlet s:'. n)}
        \ }
  let g:pytest_pdb_break_testing.o = copy(s:defuncs)
endif
