if exists('s:save_cpo')| finish| endif
let s:save_cpo = &cpo| set cpo&vim
scriptencoding utf-8
"=============================================================================
"let s:QUOTE_STARTER = '[-^~|*+,./:;<=>?@!''`"#$%&()[\]{}[:blank:]、。]' " 日本語の句読点の後のクォートを有効にしたい
let s:EMPBACK_SEPS = [',']

let s:mines = []
let s:almostvalid = {} " 設置されたのに発火条件を満たさなくなったもの. <BS>で発動条件を満たすと復活する
let s:within_butt = {} " for handle_inserted_1char_n_closing
let s:gone_butt = {} " for sep_inputted_then_turn
let s:batch_len = 0

let s:MineCommon = {}
function! s:MineCommon.CharDistanceTo(ctx) abort "{{{
  return a:ctx.Colx==0 ? 0 : strchars(a:ctx.CrrLine[self.Colx : a:ctx.Colx-1])
endfunc
"}}}
function! s:MineCommon.IsValidPos(ctx) abort "{{{
  return self.row == a:ctx.Row && a:ctx.CrrLine[: self.Colx-1] ==# self.leftlineQ
endfunc
"}}}
function! s:MineCommon.IsCrrNowCharEqualStopBgn(ctx) abort "{{{
  return a:ctx.Colx==0 ? 0 : matchstr(a:ctx.CrrLine[: a:ctx.Colx-1], '.$') ==# matchstr(self.Stop, '^.')
endfunc
"}}}
let s:QuoteMine = {}
function! s:newQuoteMine(def, ctxer) abort "{{{
  let u = extend(copy(s:QuoteMine), s:MineCommon)
  let u.row = a:ctxer.Row
  let u.leftlineQ = a:ctxer.CrrLine[: a:ctxer.Colx-1]

  let u.Colx = a:ctxer.Colx
  let u.Start = a:def.start
  let u.Stop = a:def.trig
  return u
endfunc
"}}}
function! s:QuoteMine.IsIgnitable(ctx) abort "{{{
  return a:ctx.CrrLine[a:ctx.Colx :] !~# '^\V'. escape(self.Stop, '\')
endfunc
"}}}
let s:PairMine = {}
function! s:newPairMine(def, ctxer) abort "{{{
  let u = extend(copy(s:PairMine), s:MineCommon)
  let u.row = a:ctxer.Row
  let u.leftlineQ = a:ctxer.CrrLine[: a:ctxer.Colx-1]

  let u.startx = a:def.startx
  let u.trig = a:def.trig
  let u.orgStop = a:def.stop

  let u.Colx = a:ctxer.Colx
  let u.Start = a:def.start
  let u.Stop = a:def.stop

  let u.OrgStart = a:def.start
  return u
endfunc
"}}}
function! s:PairMine.IsIgnitable(ctx) abort "{{{
  return !self.IsCrrNowCharEqualStopBgn(a:ctx)
endfunc
"}}}
function! s:PairMine.Accum(def, ctxer) abort "{{{
  let self.leftlineQ = a:ctxer.CrrLine[: a:ctxer.Colx-1]
  let self.startx = self.startx. a:def.startx
  let self.Colx = a:ctxer.Colx
  let self.Stop = self.Stop. a:def.stop
endfunc
"}}}
let s:Contexter = {}
function! s:newContexter(ctx) abort "{{{
  let u = copy(s:Contexter)
  let u.CrrLine = a:ctx.CrrLine
  let u.Row = a:ctx.Row
  return u
endfunc
"}}}
function! s:Contexter.About(trig, colx) abort "{{{
  let self.Colx = a:colx
  let l = a:colx-1-len(a:trig)
  let self.LeftLine = l <= 0 ? '' : self.CrrLine[: l]

  let self.Trig = a:trig
  let self.TrigE = escape(a:trig, '\')
  let self.RightLine = self.CrrLine[a:colx :]
  let self.PreChar = matchstr(self.LeftLine, '.$')
  return self
endfunc
"}}}
function! s:Contexter.FailInQuote(def) abort "{{{
  if self.RightLine =~# '^\V'. self.TrigE " 右隣に同じ字なく
    return 1
  elseif self.LeftLine !~# '\(^\|[^[:alnum:]_\\]\)\V'. escape(a:def.startx, '\'). '\$' " 左隣が英数など以外で始まり
    return 2
  elseif self.Colx!=0 && self.PreChar ==# self.Trig && a:def.startx !~# '\V'. self.TrigE. '\$' " 左隣に同じ字がある場合はstartxで定義されている
    return 3
  end
  return 0
endfunc
"}}}
function! s:Contexter.FailInPair(def) abort "{{{
  if self.RightLine =~# '^\w'
    return 1
  elseif self.LeftLine !~# (a:def.start =~# '^\w' ? '\(^\|[^[:alnum:]_]\)\V' : '\V'). escape(a:def.startx, '\'). '\$'
    return 2
  end
  return 0
endfunc
"}}}

function! autocloser#init() abort "{{{
  if !exists('g:autocloser_def')
    return
  end
  let [s:_c2def, s:_changedtick, s:batch_len] = [{}, b:changedtick, 0]
  let s:inhibition_pats = s:obtain_inhibition_pats()
  let items = items(g:autocloser_def)
  let items = s:parse_collectionstr(s:filter_by_buftype(items))
  let expats = s:exclusion_pat(items)
  for [key, ds] in items
    let condis = split(key, '|')
    for d in ds
      if d=~'^\s*$' || index(expats, d)!=-1
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
function! s:filter_by_buftype(items) abort "{{{
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
function! s:exclusion_pat(items) abort "{{{
  let ret = []
  for [_, ds] in a:items
    for d in ds
      if d=~'^\s\+'
        let ret += [substitute(d, '^\s*', '', '')]
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
function! autocloser#insert_pre() abort "{{{
  if !s:during_feedkeys
    let s:batch_len += 1
  end
endfunc
"}}}
function! autocloser#cleanup() abort "{{{
  let [s:mines, s:within_butt, s:almostvalid, s:during_feedkeys, s:feedkeys, s:save_row, s:save_colx] = [[], {}, {}, 0, '', 0, 0]
  unlet! s:_c2def s:inhibition_pats s:_changedtick s:batch_len s:save_row s:save_colx
endfunc
"}}}

let [s:during_feedkeys, s:save_row, s:save_colx, s:feedkeys] = [0, 0, 0, '']
function! autocloser#chk_et_append() abort "{{{
  let ctx = {'Row': line('.'), 'Colx': col('.')-1, 'CrrLine': getline('.')}
  if s:during_feedkeys && s:save_row == ctx.Row
    let targstr = ctx.CrrLine[s:save_colx : ctx.Colx-1]
    let tcolx = s:save_colx
  else
    let MATCHSTRPOS = exists('*matchstrpos') ? function('matchstrpos') : function('s:matchstrpos')
    let m = ctx.Colx==0 ? '' : MATCHSTRPOS(ctx.CrrLine[: ctx.Colx-1], '.\{'. s:batch_len. '}$', 0)
    let [targstr, tcolx] = [m[0], m[1]]
  end
  let [is_changed, during_feedkeys] = [b:changedtick != s:_changedtick, s:during_feedkeys]
  let [s:during_feedkeys, s:batch_len, s:_changedtick] = [0, 0, b:changedtick]
  if targstr!=''
    return s:caseof_input(ctx, targstr, tcolx)
  end
  return during_feedkeys ? 'feedkeys `'. s:feedkeys. '`' : s:caseof_noinput(ctx, is_changed)
endfunc
"}}}
function! s:caseof_input(ctx, targstr, tcolx) abort "{{{
  let [tcolx, feeds, ctxer] = [a:tcolx, [], s:newContexter(a:ctx)]
  if s:within_butt!={} && !(s:within_butt.IsValidPos(a:ctx) && s:within_butt.CharDistanceTo(a:ctx) <= 2)
    let s:within_butt = {}
  end
  let targchars = split(a:targstr, '\zs')
  let targlen = len(targchars)
  let i = 0
  while i < targlen-1
    let c = targchars[i]
    let tcolx += len(c)
    let def = s:obtain_def(ctxer.About(c, tcolx))
    let offset = s:offset_premine(ctxer)
    if offset
      let i += offset
      continue
    elseif def!={}
      let s:mines += [def.type==#'quote' ? s:newQuoteMine(def, ctxer) : s:newPairMine(def, ctxer)]
    end
    let i += 1
  endwhile
  let c = targchars[-1]
  let def = s:obtain_def(ctxer.About(c, a:ctx.Colx))
  let result = s:or_chain(ctxer, def, feeds,
    \ function('s:complement_premine_stop'), function('s:resemble_premine'), function('s:handle_inserted_1char_n_closing'), function('s:set_mine'), function('s:sep_inputted_then_turn'))
  call s:ignite_mines(ctxer, feeds)
  if feeds==[]
    return result
  end
  let str = join(feeds, '')
  if str!=''
    let [s:during_feedkeys, s:save_row, s:save_colx, s:feedkeys] = [1, a:ctx.Row, a:ctx.Colx, str]
    call feedkeys(str, 'n')
  end
  return result
endfunc
"}}}
function! s:caseof_noinput(ctx, is_changed) abort "{{{
  let [s:within_butt, s:gone_butt, s:mines] = [{}, {}, []]
  if !(a:is_changed && s:almostvalid!={} && s:almostvalid.IsValidPos(a:ctx))
    let s:almostvalid = {}
  elseif s:almostvalid.IsIgnitable(a:ctx) && a:ctx.Colx == s:almostvalid.Colx
    let s:mines += [s:almostvalid]
    let s:almostvalid = {}
  end
  return 'targchars is zero: <CR>,<BS>, or moved cursor'
endfunc
"}}}

function! s:offset_premine(ctxer) abort "{{{
  if s:mines==[] || a:ctxer.RightLine !~# '^\V'. escape(s:mines[-1].Stop, '\')
    return 0
  end
  let mine = remove(s:mines, -1)
  return strchars(mine.Stop)
endfunc
"}}}
function! s:complement_premine_stop(ctxer, _, feeds) abort "{{{
  if s:mines==[] || matchstr(s:mines[-1].Stop, '^.') !=# a:ctxer.Trig
    return ''
  end
  let mine = remove(s:mines, -1)
  call add(a:feeds , substitute(mine.Stop, '^.', '', ''))
  let [s:gone_butt, s:almostvalid] = [mine, mine]
  return 'handle_premine: complemented closing'
endfunc
"}}}
function! s:resemble_premine(ctxer, def, _) abort "{{{
  if s:mines==[] || a:def=={}
    return ''
  end
  let mine = s:mines[-1]
  if get(mine, 'OrgStart', '') ==# a:def.start " 同じstartを持つなら重ねる
    call mine.Accum(a:def, a:ctxer)
    return 'handle_premine: accumulated'
  elseif matchstr(mine.Start, '^.') ==# matchstr(a:def.start, '^.') " 開始文字が同じ場合、復活の可能性
    let s:almostvalid = remove(s:mines, -1)
  end
  return ''
endfunc
"}}}
function! s:handle_inserted_1char_n_closing(ctxer, _, feeds) abort "{{{
  if s:within_butt=={} || !(matchstr(s:within_butt.Stop, '^.') ==# a:ctxer.Trig && a:ctxer.RightLine =~# '^\V'. escape(s:within_butt.Stop, '\'))
    return ''
  end
  if s:mines!=[] && s:mines[-1].Colx == a:ctxer.Colx-1 " '`"`' などの入れ子になったトリガ対策
    call remove(s:mines, -1)
  end
  call add(a:feeds , "\<C-g>U\<BS>". repeat("\<C-g>U\<Right>", strchars(s:within_butt.Stop)))
  let s:within_butt = {}
  return 'handle_inserted_1char_n_closing'
endfunc
"}}}
function! s:set_mine(ctxer, def, feeds) abort "{{{
  if a:def=={}
    return ''
  end
  let s:mines += [a:def.type==#'quote' ? s:newQuoteMine(a:def, a:ctxer) : s:newPairMine(a:def, a:ctxer)]
  return 'set_mine'
endfunc
"}}}
function! s:sep_inputted_then_turn(ctxer, _, feeds) abort "{{{
  if s:gone_butt=={} || !g:autocloser_enable_separator_turnback
    return ''
  elseif !(index(s:EMPBACK_SEPS, a:ctxer.Trig)!=-1 && s:gone_butt.IsValidPos(a:ctxer) && a:ctxer.LeftLine =~# '\V'. escape(s:gone_butt.Start. s:gone_butt.Stop, '\'). '\$')
    let s:gone_butt = {}
    return ''
  end
  call add(a:feeds , repeat("\<C-g>U\<Left>", strchars(s:gone_butt.Stop. a:ctxer.Trig)))
  let s:gone_butt = {}
  return 'sep_inputted_then_turn'
endfunc
"}}}
function! s:ignite_mines(ctxer, feeds) abort "{{{
  for pat in s:inhibition_pats
    if search(pat, 'bcWn')
      call filter(s:mines, 'v:val.Colx == a:ctxer.Colx')
      return
    end
  endfor
  let [lenM, is_first] = [len(s:mines), 1]
  while !(s:mines==[] || s:mines[0].Colx == a:ctxer.Colx)
    let mine = remove(s:mines, 0)
    if !mine.IsValidPos(a:ctxer)
    elseif !(mine.IsIgnitable(a:ctxer) || lenM > 1 && is_first)
      let s:almostvalid = mine
    else
      call add(a:feeds, mine.Stop. repeat("\<C-g>U\<Left>", strchars(mine.Stop)))
      let s:within_butt = mine
    end
    let is_first = 0
  endwhile
endfunc
"}}}



function! s:or_chain(ctxer, def, feeds, ...) abort "{{{
  for F in a:000
    let ret = F(a:ctxer, a:def, a:feeds)
    if ret!=''
      return ret
    end
  endfor
  return 'no operation'
endfunc
"}}}
function! s:obtain_inhibition_pats() abort "{{{
  let pats = []
  let [fts, bext] = [split(&ft, '\.'), expand('%:e')]
  for [key, pat] in items(g:autocloser_inhibition_pat)
    for ftpat in split(key, '|')
      if (ftpat =~ '^.' && bext ==? ftpat[1:]) || match(fts, substitute(ftpat, '\*', '.\\+', 'g'))!=-1
        let pats += [pat]
      end
    endfor
  endfor
  return pats
endfunc
"}}}
function! s:obtain_def(ctxer) abort "{{{
  if !has_key(s:_c2def, a:ctxer.Trig)
    return {}
  end
  let dfs1 = []
  "let [fts, bext] = [split(&ft, '\.'), expand('%:e')]
  for def in s:_c2def[a:ctxer.Trig]
    for condi in def.condis
      "if ((condi =~ '^.' && bext ==? condi[1:]) || match(fts, substitute(condi, '\*', '.\\+', 'g'))!=-1)
      if !a:ctxer[def.type=="quote" ? 'FailInQuote' : 'FailInPair'](def)
        let dfs1 += [{'len': len(def.startx), 'condi': condi, 'type': def.type, 'def': def}]
        break
      end
      "end
    endfor
  endfor
  if dfs1==[]
    return {}
  end
  call sort(dfs1, 's:_sort_df')
  return dfs1[-1].def
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
function! s:matchstrpos(str, pat, start) abort "{{{
  let [bgn, end] = [match(a:str, a:pat, a:start), matchend(a:str, a:pat, a:start)]
  return [a:str[bgn : end-1], bgn, end]
endfunc
"}}}
"=============================================================================
"END "{{{1
let &cpo = s:save_cpo| unlet s:save_cpo
