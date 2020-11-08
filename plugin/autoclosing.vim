if expand('<sfile>:p')!=#expand('%:p') && exists('g:loaded_autoclosing')| finish| endif| let g:loaded_autoclosing = 1
let s:save_cpo = &cpo| set cpo&vim
scriptencoding utf-8
"=============================================================================
let g:autoclosing_def = exists('g:autoclosing_def') ? g:autoclosing_def : {"*": ['"', "'", '`', '( )', '[ ]', '{ }'], 'vim': [' `']}
let g:autoclosing_inhibition_pat = exists('g:autoclosing_inhibition_pat') ? g:autoclosing_inhibition_pat : {'vim': '" \%#$\|^\s*".\%#'}

let g:autoclosing_enable_separator_turnback = 0

aug autoclosing
  au!
  au InsertEnter *  call autoclosing#init()
  au InsertLeave *  call autoclosing#cleanup()
  au InsertCharPre *  call autoclosing#insert_pre()
  au CursorMovedI * call autoclosing#chk_et_append()
aug END
"=============================================================================
"END "{{{1
let &cpo = s:save_cpo| unlet s:save_cpo
