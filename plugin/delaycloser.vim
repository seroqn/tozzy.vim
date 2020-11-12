if expand('<sfile>:p')!=#expand('%:p') && exists('g:loaded_delaycloser')| finish| endif| let g:loaded_delaycloser = 1
let s:save_cpo = &cpo| set cpo&vim
scriptencoding utf-8
"=============================================================================
let g:delaycloser_def = exists('g:delaycloser_def') ? g:delaycloser_def : {"*": ['"', "'", '`', '( )', '[ ]', '{ }'], 'vim': [' `']}
let g:delaycloser_inhibition_pat = exists('g:delaycloser_inhibition_pat') ? g:delaycloser_inhibition_pat : {'vim': '^\s*".\%#\|" \%#$'}

aug delaycloser
  au!
  au InsertEnter *  call delaycloser#init()
  au InsertLeave *  call delaycloser#cleanup()
  au InsertCharPre *  call delaycloser#insert_pre()
  au CursorMovedI * call delaycloser#chk_et_append()
aug END
"=============================================================================
"END "{{{1
let &cpo = s:save_cpo| unlet s:save_cpo
