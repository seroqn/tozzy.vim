if expand('<sfile>:p')!=#expand('%:p') && exists('g:loaded_autoclosing')| finish| endif| let g:loaded_autoclosing = 1
let s:save_cpo = &cpo| set cpo&vim
scriptencoding utf-8
"=============================================================================
let g:autoclosing_def = exists('g:autoclosing_def') ? g:autoclosing_def : {"*": ['"', "'", '`', '( )', '[ ]', '{ }'], 'javascript|typescript': ['`', '/']}
let g:autoclosing_enable_separator_turnback = 0
let g:autoclosing_def = {'add': {"*": ['"', "'", '`', '( )', '[ ]', '{ }'], 'javascript|typescript': ['`', '/']}}
let g:autoclosing_def_removal = {}
"let g:autoclosing_def = {"*": {"quotes": ['"', "'", '％', '¶'], "pairs": ['( )', '[ ]', '{ }', '「 」']}}
"let g:autoclosing_cmode_def = exists('g:autoclosing_cmode_def') ? g:autoclosing_cmode_def : {"*": ['"', "'", '`', '( )', '[ ]', '{ }']}
" cmodeでの設定はとりあえず変えられない(設定固定)ようにして、要望があったら変更可能にする
let g:autoclosing_def = {"*": ['"', "'", '`', '＠', '( )', '[ ]', '{ }', '「 」', '（ ）', '｛ ｝']}
let g:autoclosing_def["vim"] = [' `']
let g:autoclosing_def["html|*react|.jsx"] = ['< >']
let g:autoclosing_def[".ejs"] = ['<% %>', '<%&&[=\-#%] %>']
let g:autoclosing_def["javascript"] = ['`']

let g:autoclosing_def["python"] = ["f'", '&&[frbu]&&["'']']
"let g:autoclosing_def["html"] = ["<!--  -->", "/**  */"]
let g:autoclosing_def["ruby"] = ["%Q!", "%Q{ }", '%&&[qQwWiIxs]&&[!`]', '%&&[qQwWiIxs]{ }', '%&&[qQwWiIxs]< >']

let g:autoclosing_inhibition_pat = {'vim': '" \%#$\|^\s*".\%#'} " 
inoremap <silent><expr><Plug>(autoclosing-break)  autoclosing#break()
"command! -nargs=0   autoclosingReloadConfig   unlet! b:autoclosing_is_startated s:loaded_global | call s:init()

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
