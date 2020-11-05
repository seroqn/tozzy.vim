if exists('s:save_cpo')| finish| endif
let s:save_cpo = &cpo| set cpo&vim
scriptencoding utf-8
"=============================================================================
"let s:QUOTE_STARTER = '[-^~|*+,./:;<=>?@!''`"#$%&()[\]{}[:blank:]、。]' " 日本語の句読点の後のクォートを有効にしたい
let s:EMPBACK_SEPS = [',']

let s:infos = [] " insert関数で加えられ次の一字入力時(X)のCursorMovedIで吐き出される. Xが新たなトリガである可能性を考え長さ2のFIFO. 長さ2に達するのはinsert関数終了後CursorMovedI途中までの間だけである
let s:ignitees = []
let s:bs_revivable = {} " 設置されたのに発火条件を満たさなくなったもの. <BS>で発動条件を満たすと復活する
let s:onecharbreak = {} " 発火した後、次の一字入力までの間生きる。insert関数で優先的に閉じを見る
let s:char_after_place = -1 " 1 以上でIMからの複数文字一度の入力と見なす. 1 以下が onecharbreak の発動条件
function! s:append_info(info) abort "{{{
  let s:infos += [a:info]
  let s:char_after_place = 0
endfunc
"}}}
function! s:putback_or_clear_bs_revivable(is_changed, ctxt) abort "{{{
  if s:bs_revivable=={} || !a:is_changed || !s:bs_revivable.IsValid(a:ctxt)
    let s:bs_revivable = {}
    return
  elseif !(s:bs_revivable.IsIgnitable(a:ctxt) && a:ctxt.colx == s:bs_revivable.Colx)
    return
  end
  call s:append_info(s:bs_revivable)
  let s:bs_revivable = {}
endfunc
"}}}
let s:InfoCommon = {}
function! s:InfoCommon.CharDistanceTo(ctxt) abort "{{{
  return strchars(a:ctxt.crrline[self.Colx : a:ctxt.colx-1])
endfunc
"}}}
function! s:InfoCommon.IsValid(ctxt) abort "{{{
  return self.row == a:ctxt.row && a:ctxt.crrline[:self.Colx-1] ==# self.leftline
endfunc
"}}}
function! s:InfoCommon.IsCrrPrecharEqualStopBgn(ctxt) abort "{{{
  return matchstr(a:ctxt.crrline[: a:ctxt.colx-1], '.$') ==# matchstr(self.Stop, '^.')
endfunc
"}}}
let s:QuoteInfo = {}
function! s:newQuoteInfo(def) abort "{{{
  let u = extend(copy(s:QuoteInfo), s:InfoCommon)
  let colx = col('.')-1
  let u.row = line('.')
  let u.leftline = (colx==0 ? '' : getline('.')[: colx-1]). a:def.trig

  let u.Colx = colx + len(a:def.trig)
  let u.Start = a:def.start
  let u.Stop = a:def.trig
  let u.IgnitionFeeds = ''
  let u.IgnitionStepnum = -1
  return u
endfunc
"}}}
function! s:QuoteInfo.IsIgnitable(ctxt) abort "{{{
  return a:ctxt.crrline[self.Colx:] !~# '^\V'. escape(self.Stop, '\')
endfunc
"}}}
function! s:QuoteInfo.Ignite(ctxt, skip_calcof_colx, stepnum) abort "{{{
  if !(a:skip_calcof_colx || self.CharDistanceTo(a:ctxt) == 1) || !(a:stepnum==-1 || self.IgnitionStepnum == a:stepnum) || s:should_inhibit()
    return -1
  end
  return feedkeys(self.IgnitionFeeds == '' ? s:str_nomovify(self.Stop) : self.IgnitionFeeds, 'n')
endfunc
"}}}
let s:PairInfo = {}
function! s:newPairInfo(def) abort "{{{
  let u = extend(copy(s:PairInfo), s:InfoCommon)
  let colx = col('.')-1
  let u.row = line('.')
  let u.leftline = (colx==0 ? '' : getline('.')[: colx-1]). a:def.trig

  let u.startx = a:def.startx
  let u.trig = a:def.trig
  let u.orgStop = a:def.stop

  let u.Colx = colx + len(a:def.trig)
  let u.Start = a:def.start
  let u.Stop = a:def.stop
  let u.IgnitionFeeds = ''
  let u.IgnitionStepnum = -1

  let u.OrgStart = a:def.start
  return u
endfunc
"}}}
function! s:PairInfo.IsIgnitable(ctxt) abort "{{{
  return !self.IsCrrPrecharEqualStopBgn(a:ctxt)
endfunc
"}}}
function! s:PairInfo.Ignite(ctxt, skip_calcof_colx, stepnum) abort "{{{
  if !(a:skip_calcof_colx || self.CharDistanceTo(a:ctxt) == 1) || !(a:stepnum==-1 || self.IgnitionStepnum == a:stepnum) || s:should_inhibit()
    return -1
  end
  return feedkeys(self.IgnitionFeeds == '' ? s:str_nomovify(self.Stop) : self.IgnitionFeeds, 'n')
endfunc
"}}}
function! s:PairInfo.Accum(def) abort "{{{
  let self.Colx = col('.')-1 + len(self.trig)
  let self.startx = self.startx. a:def.startx
  let self.Stop = self.Stop. a:def.stop
endfunc
"}}}
let s:DefInspector = {}
function! s:newDefInspector(crrline, colx, trig) abort "{{{
  let u = copy(s:DefInspector)
  let u.trig = a:trig
  let u.e_trig = escape(a:trig, '\')
  let u.crrline = a:crrline
  let u.colx = a:colx
  let u.leftline = a:crrline[: a:colx-1]
  let u.rightline = a:crrline[a:colx :]
  let u.prechar = s:prechar_of(a:crrline, a:colx)
  return u
endfunc
"}}}
function! s:DefInspector.fail_in_quote(def) abort "{{{
  if self.rightline =~# '^\V'. self.e_trig " 右隣に同じ字なく
    return 1
  elseif self.leftline !~# '\(^\|[^[:alnum:]_\\]\)\V'. escape(a:def.startx, '\'). '\$' " 左隣が英数など以外で始まり
    return 2
  elseif self.colx!=0 && self.prechar ==# self.trig && a:def.startx !~# '\V'. self.e_trig. '\$' " 左隣に同じ字がある場合はstartxで定義されている
    return 3
  end
  return 0
endfunc
"}}}
function! s:DefInspector.fail_in_pair(def) abort "{{{
  if self.rightline =~# '^\w'
    return 1
  elseif self.leftline !~# (a:def.start =~# '^\w' ? '\(^\|[^[:alnum:]_]\)\V' : '\V'). escape(a:def.startx, '\'). '\$'
    return 2
  end
  return 0
endfunc
"}}}

function! autocloser#init() abort "{{{
  if !exists('g:autocloser_def')
    return
  end
  let [s:_c2def, s:_changedtick, s:char_after_place] = [{}, b:changedtick, -1]
  let items = items(g:autocloser_def)
  for [key, ds] in s:parse_collectionstr(s:filter_defitems(items))
    let condis = split(key, '|')
    for d in ds
      if d=~'^\s*$'
        continue
      end
      let sep = stridx(d, ' ')
      if sep == 0
        continue
      elseif sep == -1
        let ms = matchlist(d, '^\(.*\)\(.\)$')
        let trig = ms[2]
        let s:_c2def[trig] = get(s:_c2def, trig, []) + [{'condis': condis, 'startx': ms[1], 'trig': trig, 'start': d, 'type': 'quote'}]
        continue
      end
      let [start, stop] = [d[:sep-1], d[sep+1:]]
      let ms = matchlist(start, '^\(.*\)\(.\)$')
      let trig = ms[2]
      let s:_c2def[trig] = get(s:_c2def, trig, []) + [{'condis': condis, 'startx': ms[1], 'trig': trig, 'start': start, 'stop': stop, 'type': 'pair'}]
    endfor
  endfor
endfunc
"}}}
function! s:filter_defitems(items) abort "{{{
  let ret = []
  let [fts, bext] = [split(&ft, '\.'), expand('%:e')]
  for item in a:items
    for condi in split(item[0], '|')
      if ((condi =~ '^.' && bext ==? condi[1:]) || match(fts, substitute(condi, '\*', '.\\+', 'g'))!=-1)
        let ret += [item]
      end
    endfor
  endfor
  return ret
endfunc
"}}}
function! s:parse_collectionstr(items) abort "{{{
  let MATCHSTRPOS = exists('*matchstrpos') ? function('matchstrpos') : function('s:matchstrpos')
  for [_, ds] in a:items
    let [i, lenD] = [0, len(ds)]
    while i < lenD
      if ds[i] !~ '&&['
        let i += 1
        continue
      end
      let parsees = s:def_to_parsees(remove(ds, i), MATCHSTRPOS)
      call extend(ds, parsees, i)
      let lenP = len(parsees)
      let i += lenP
      let lenD += lenP-1
    endwhile
  endfor
  return a:items
endfunc
"}}}
function! s:def_to_parsees(def, MATCHSTRPOS) abort "{{{
  let collections = []
  let result = a:MATCHSTRPOS(a:def, '&&\[\zs.\{-}\ze\%([^\\]\\\)\@<!]', 0)
  while result[1] != -1
    let collections += [result[0]]
    let result = a:MATCHSTRPOS(a:def, '&&\[\zs.\{-}\ze\%([^\\]\\\)\@<!]', result[2])
  endwhile
  call map(collections, 'split(v:val, ''\\\@<!\|\%(\\.\)\@<='')')
  let fmt = substitute(substitute(a:def, '%', '%%', 'g'), '&&\[.\{-}\%([^\\]\\\)\@<!]', '%s', 'g')
  let ret = []
  for args in s:pile(collections, len(collections)-1)
    let ret += [call('printf', [fmt] + args)]
  endfor
  return ret
endfunc
"}}}
function! s:pile(collections, i) abort "{{{
  if a:i == 0
    return map(a:collections[a:i], '[substitute(v:val, ''^\\'', "", "")]')
  end
  let ret = []
  for a in s:pile(a:collections, a:i-1)
    for c in a:collections[a:i]
      let ret += [a + [substitute(c, '^\\', '', '')]]
    endfor
  endfor
  return ret
endfunc
"}}}
function! s:matchstrpos(str, pat, start) abort "{{{
  let [bgn, end] = [match(a:str, a:pat, a:start), matchend(a:str, a:pat, a:start)]
  return [a:str[bgn : end-1], bgn, end]
endfunc
"}}}
function! autocloser#cleanup() abort "{{{
  let [s:infos, s:ignitees, s:bs_revivable, s:during_feedkeys] = [[], [], {}, 0]
  unlet! s:_c2def s:_changedtick s:char_after_place
endfunc
"}}}

let s:during_feedkeys = 0
" Main:
function! autocloser#chk_et_append() abort "{{{
  let noiminput = s:char_after_place > 0 ? 0 : s:char_after_place==0 ? -1 : 1
  let ctxt = {'row': line('.'), 'colx': col('.')-1, 'crrline': getline('.')}
  if s:ignitees!=[]
    let s:during_feedkeys = 1
    while s:ignitees!=[]
      call remove(s:ignitees, 0).Ignite(ctxt, !noiminput, s:char_after_place)
    endwhile
    return 'fullfill ignitees'
  end
  if s:onecharbreak!={} && !(s:onecharbreak.IsValid(ctxt) && (noiminput || s:onecharbreak.CharDistanceTo(ctxt) == 1))
    let s:onecharbreak = {}
  end
  let is_changed = b:changedtick != s:_changedtick
  let [s:during_feedkeys, s:char_after_place, s:_changedtick] = [0, 0, b:changedtick]
  if s:infos==[]
    call s:putback_or_clear_bs_revivable(is_changed, ctxt)
    return "noinfo"
  elseif noiminput==-1
    return "first visit after info added"
  elseif !is_changed
    call remove(s:infos, 0)
    return "clear for cursor"
  end
  if noiminput
    let result = s:ignite(s:infos[0], ctxt, 0)
    if result!=1
      return result==0 ? 'first visit' : 'fail to ignite'
    end
  else
    for info in s:infos
      let result = s:ignite(info, ctxt, 1)
      if result==0
        return 'first visit'
      elseif result==-1
        let s:infos = []
        return 'fail to ignite'
      end
    endfor
  end
  return "ignited"
endfunc
"}}}
function! s:ignite(info, ctxt, skip_calcof_colx) abort "{{{
  if !a:info.IsValid(a:ctxt) " 複数のleftについて<BS>している可能性を考えもう一度 validateする
    call remove(s:infos, 0)
    return -1
  end
  if !a:info.IsIgnitable(a:ctxt)
  "if a:info.IsCrrPrecharEqualStopBgn(a:ctxt)
    if a:info.Colx == a:ctxt.colx
      return 0
    end
    call remove(s:infos, 0)
    let s:bs_revivable = a:info
    return -1
  end
  call remove(s:infos, 0)
  call a:info.Ignite(a:ctxt, a:skip_calcof_colx, -1)
  let s:during_feedkeys = 1
  let s:onecharbreak = a:info
  return 1
endfunc
"}}}

function! autocloser#insert_pre(trig) abort "{{{
  if s:during_feedkeys
    return
  end
  let s:char_after_place = s:char_after_place==-1 ? -1 : s:char_after_place + 1

  let [colx, crrline] = [col('.')-1, getline('.')]
  if s:turn_by_inserting_separator(a:trig, crrline, colx)
    return
  end
  if s:at_inserting_1char_n_stopstr(a:trig, crrline, colx)
    return
  end
  "if s:at_inserting_stopstr(a:trig, crrline, colx)
    "return
  "end
  if has_key(s:_c2def, a:trig)
    call s:place_info(a:trig, s:_c2def[a:trig], s:newDefInspector(crrline, colx, a:trig))
  end
endfunc
"}}}
function! s:place_info(trig, defs, inspector) abort "{{{
  let dfs1 = []
  let [fts, bext] = [split(&ft, '\.'), expand('%:e')]
  for def in a:defs
    for condi in def.condis
      "if ((condi =~ '^.' && bext ==? condi[1:]) || match(fts, substitute(condi, '\*', '.\\+', 'g'))!=-1)
        if !a:inspector[def.type=="quote" ? 'fail_in_quote' : 'fail_in_pair'](def)
          let dfs1 += [{'len': len(def.startx), 'condi': condi, 'type': def.type, 'def': def}]
          break
        end
      "end
    endfor
  endfor
  if dfs1==[]
    return
  end
  call sort(dfs1, 's:_sort_df')
  let def = dfs1[-1].def
  if s:oldinfo(def)
    return
  end
  "if def.type!=#'quote' " TODO
    "return 0
  "end
  call s:append_info(def.type==#'quote' ? s:newQuoteInfo(def) : s:newPairInfo(def))
endfunc
"}}}
function! s:turn_by_inserting_separator(trig, crrline, colx) abort "{{{
  if s:bs_revivable=={} || index(s:EMPBACK_SEPS, a:trig)==-1
    return 0
  elseif !(s:bs_revivable.IsValid({'row': line('.'), 'crrline': a:crrline}) && a:crrline[: a:colx-1] =~# '\V'. escape(s:bs_revivable.Stop, '\'). '\$')
    return 0
  end
  let info = copy(s:bs_revivable)
  let info.IgnitionFeeds = repeat("\<C-g>U\<Left>", strchars(s:bs_revivable.Stop. a:trig))
  let info.IgnitionStepnum = s:char_after_place
  let s:ignitees += [info]
  let s:bs_revivable = {}
  return 1
endfunc
"}}}
function! s:at_inserting_1char_n_stopstr(trig, crrline, colx) abort "{{{
  if s:onecharbreak=={} || s:char_after_place > 1
    let s:onecharbreak = {}
    return 0
  elseif !(matchstr(s:onecharbreak.Stop, '^.') ==# a:trig && a:crrline[a:colx :] =~# '^\V'. escape(s:onecharbreak.Stop, '\'))
    return 0
  end
  let info = copy(s:onecharbreak)
  let info.IgnitionFeeds = "\<C-g>U\<BS>". repeat("\<C-g>U\<Right>", strchars(info.Stop))
  let info.IgnitionStepnum = s:char_after_place
  let s:ignitees += [info]
  let s:onecharbreak = {}
  return 1
endfunc
"}}}
function! s:at_inserting_stopstr(trig, crrline, colx) abort "{{{
  if s:infos==[] || matchstr(s:infos[-1].Stop, '^.') !=# a:trig
    return 0
  end
  let info = remove(s:infos, -1)
  let info.IgnitionFeeds = "\<C-g>U\<BS>". info.Stop
  let info.IgnitionStepnum = s:char_after_place
  let s:ignitees += [info]
  return 1
endfunc
"}}}
function! s:oldinfo(def) abort "{{{
  if s:infos==[]
    return 0
  end
  let info = s:infos[-1]
  if matchstr(info.Stop, '^.') ==# a:def.trig " 入力文字が古いinfoの終了文字になってるなら古いinfo優先
    let info.IgnitionFeeds = substitute(info.Stop, '^.', '', '')
    let info.IgnitionStepnum = s:char_after_place
    let s:ignitees += [remove(s:infos, -1)]
    return 2
  elseif get(info, 'OrgStart', '') ==# a:def.start " 同じstartを持つなら重ねる
    call info.Accum(a:def)
    let s:char_after_place = 0
    return 1
  elseif matchstr(info.Start, '^.') ==# matchstr(a:def.start, '^.') " 開始文字が同じ場合、復活の可能性
    let s:bs_revivable = remove(s:infos, -1)
  end
  return 0
endfunc
"}}}




function! s:str_nomovify(str) abort "{{{
  return a:str. repeat("\<C-g>U\<Left>", strchars(a:str))
endfunc
"}}}
function! s:str_breakify(str) abort "{{{
  return a:str. repeat("\<C-g>U\<Right>", strchars(a:str))
endfunc
"}}}
function! s:should_inhibit() abort "{{{
  let [fts, bext] = [split(&ft, '\.'), expand('%:e')]
  for [key, pat] in items(g:autocloser_inhibition_pat)
    for ftpat in split(key, '|')
      if !((ftpat =~ '^.' && bext ==? ftpat[1:]) || match(fts, substitute(ftpat, '\*', '.\\+', 'g'))!=-1)
        continue
      end
      if search(pat, 'bcWn')
        return 1
      end
    endfor
  endfor
  return 0
endfunc
"}}}
function! s:prechar_of(line, colx) abort "{{{
  return matchstr(a:line[: a:colx-1], '.$')
endfunc
"}}}
function! s:_sort_df(a, b) abort "{{{
  if a.condi != b.condi
    let ret = a.condi=='*' ? -1 : b.condi=='*' ? 1 : 0
    if ret
      return ret
    end
  end
  let ret = a.len - b.len
  return ret || a.type==#b.type ? ret : a.type=='pair' ? -1 : 1
endfunc
"}}}
function! autocloser#__warning(msg) abort "{{{
  echoh Error
  echom "autocloser.vim: ". a:msg
  echoh NONE
endfunc
"}}}
"=============================================================================
"END "{{{1
let &cpo = s:save_cpo| unlet s:save_cpo
