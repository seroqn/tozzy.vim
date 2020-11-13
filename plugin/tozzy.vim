if expand('<sfile>:p')!=#expand('%:p') && exists('g:loaded_tozzy')| finish| endif| let g:loaded_tozzy = 1
let s:save_cpo = &cpo| set cpo&vim
scriptencoding utf-8
"=============================================================================
let g:tozzy_def = exists('g:tozzy_def') ? g:tozzy_def : {"*": ['"', "'", '`', '( )', '[ ]', '{ }'], 'vim': [' `']}
let g:tozzy_inhibition_pat = exists('g:tozzy_inhibition_pat') ? g:tozzy_inhibition_pat : {'vim': '^\s*".\%#\|" \%#$'}

let s:is_inited = 0
aug tozzy
  au!
  au InsertEnter *  call tozzy#init() | let s:is_inited = 1
  au InsertLeave *  call tozzy#cleanup()
  au InsertCharPre *  call tozzy#insert_pre()
  au CursorMovedI * call tozzy#chk_et_append()
  if exists('##SafeState')
    au SafeState * if s:is_inited | call tozzy#safestate() | endif
  end
aug END
"=============================================================================
"END "{{{1
let &cpo = s:save_cpo| unlet s:save_cpo
