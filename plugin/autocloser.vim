if expand('<sfile>:p')!=#expand('%:p') && exists('g:loaded_autocloser')| finish| endif| let g:loaded_autocloser = 1
let s:save_cpo = &cpo| set cpo&vim
scriptencoding utf-8
"=============================================================================
let g:autocloser_def = exists('g:autocloser_def') ? g:autocloser_def : {"*": ['"', "'", '`', '( )', '[ ]', '{ }'], 'javascript|typescript': ['`', '/']}
let g:autocloser_enable_separator_turnback = 0
let g:autocloser_def = {'add': {"*": ['"', "'", '`', '( )', '[ ]', '{ }'], 'javascript|typescript': ['`', '/']}}
let g:autocloser_def_removal = {}
"let g:autocloser_def = {"*": {"quotes": ['"', "'", '％', '¶'], "pairs": ['( )', '[ ]', '{ }', '「 」']}}
"let g:autocloser_cmode_def = exists('g:autocloser_cmode_def') ? g:autocloser_cmode_def : {"*": ['"', "'", '`', '( )', '[ ]', '{ }']}
" cmodeでの設定はとりあえず変えられない(設定固定)ようにして、要望があったら変更可能にする
let g:autocloser_def = {"*": ['"', "'", '`', '＠', '( )', '[ ]', '{ }', '「 」', '（ ）', '｛ ｝']}
let g:autocloser_def["vim"] = [' `']
let g:autocloser_def["html|*react|.jsx"] = ['< >']
let g:autocloser_def[".ejs"] = ['<% %>', '<%&&[=\-#%] %>']
let g:autocloser_def["javascript"] = ['`']

let g:autocloser_def["python"] = ["f'", '&&[frbu]&&["'']']
"let g:autocloser_def["html"] = ["<!--  -->", "/**  */"]
let g:autocloser_def["ruby"] = ["%Q!", "%Q{ }", '%&&[qQwWiIxs]&&[!`]', '%&&[qQwWiIxs]{ }', '%&&[qQwWiIxs]< >']

let g:autocloser_inhibition_pat = {'vim': '" \%#$\|^\s*".\%#'} " 
inoremap <silent><expr><Plug>(autocloser-break)  autocloser#break()
"command! -nargs=0   autocloserReloadConfig   unlet! b:autocloser_is_startated s:loaded_global | call s:init()

aug autocloser
  au!
  au InsertEnter *  call autocloser#init()
  au InsertLeave *  call autocloser#cleanup()
  au InsertCharPre *  call autocloser#insert_pre()
  au CursorMovedI * call autocloser#chk_et_append()
aug END

"=============================================================================
"END "{{{1
let &cpo = s:save_cpo| unlet s:save_cpo
