if exists('s:save_cpo')| finish| endif
let s:save_cpo = &cpo| set cpo&vim
scriptencoding utf-8
"=============================================================================
let s:EMPBACK_SEPS = [',']
let s:mines = []
let s:almostvalid = {} " 設置されたのに発火条件を満たさなくなったもの. <BS>で発動条件を満たすと復活する
let s:within_butt = {} " for handle_inserted_1char_n_closing
let s:gone_butt = {} " for sep_inputted_then_turn
let s:batch_len = 0

function! autoclosing#is_leavable() abort "{{{
  return s:Leavablee.GetRhs()!=''
endfunc
"}}}
function! autoclosing#leave() abort "{{{
  let rhs = s:Leavablee.GetRhs()
  call s:Leavablee.Reset()
  return rhs
endfunc
"}}}

let s:Leavablee = {'closing': '', 'rightline': ''}
function! s:Leavablee.Reset() abort "{{{
  let [self.closing, self.rightline] = ['', '']
  return self
endfunc
"}}}
function! s:Leavablee.Store(closing, ctxer) abort "{{{
  let self.rightline = a:ctxer.RightLine[len(self.closing) :]
  let self.closing = a:closing. self.closing
endfunc
"}}}
function! s:Leavablee.GetRhs() abort "{{{
  if self.closing == ''
    return ''
  end
  let colx = col('.')-1
  let rightline = colx==0 ? getline('.') : getline('.')[colx :]
  let idx = stridx(rightline, self.closing)
  if idx==-1 || rightline[idx+len(self.closing) :] !=# self.rightline
    return ''
  end
  return repeat("\<C-g>U\<Right>", strchars((idx==0 ? '' : rightline[: idx-1]). self.closing))
endfunc
"}}}
let s:MineCommon = {'Row': 0, 'Colx': 0, 'leftlineQ': '', 'Closing': ''}
function! s:MineCommon.CharDistanceTo(ctx) abort "{{{
  return a:ctx.Colx==0 ? 0 : strchars(a:ctx.CrrLine[self.Colx : a:ctx.Colx-1])
endfunc
"}}}
function! s:MineCommon.IsValidPos(ctx) abort "{{{
  return self.Row == a:ctx.Row && a:ctx.CrrLine[: self.Colx-1] ==# self.leftlineQ
endfunc
"}}}
function! s:MineCommon.IsCrrNowCharEqualClosingBgn(ctx) abort "{{{
  return a:ctx.Colx==0 ? 0 : matchstr(a:ctx.CrrLine[: a:ctx.Colx-1], '.$') ==# matchstr(self.Closing, '^.')
endfunc
"}}}
let s:QuoteMine = {}
function! s:newQuoteMine(def, ctxer) abort "{{{
  let u = extend(copy(s:QuoteMine), s:MineCommon, 'keep')
  let u.leftlineQ = a:ctxer.CrrLine[: a:ctxer.Colx-1]

  let u.IsImidiate = a:def.IsImidiate
  let u.Row = a:ctxer.Row
  let u.Colx = a:ctxer.Colx
  let u.Opening = a:def.Opening
  let u.Closing = a:def.Opening
  return u
endfunc
"}}}
function! s:QuoteMine.IsIgnitable(ctx) abort "{{{
  return a:ctx.CrrLine[a:ctx.Colx :] !~# '^\V'. escape(self.Closing, '\')
endfunc
"}}}
function! s:QuoteMine.CanAppendNL(_) abort "{{{
  return 0
endfunc
"}}}
let s:PairMine = {}
function! s:newPairMine(def, ctxer) abort "{{{
  let u = extend(copy(s:PairMine), s:MineCommon, 'keep')
  let u.leftlineQ = a:ctxer.CrrLine[: a:ctxer.Colx-1]

  let u.IsImidiate = a:def.IsImidiate
  let u.Row = a:ctxer.Row
  let u.Colx = a:ctxer.Colx
  let u.Opening = a:def.Opening
  let u.Closing = a:def.Closing
  let u.OrgOpening = a:def.Opening
  return u
endfunc
"}}}
function! s:PairMine.IsIgnitable(ctx) abort "{{{
  return !self.IsCrrNowCharEqualClosingBgn(a:ctx)
endfunc
"}}}
function! s:PairMine.Accum(def, ctxer) abort "{{{
  let self.leftlineQ = a:ctxer.CrrLine[: a:ctxer.Colx-1]
  let self.Colx = a:ctxer.Colx
  let self.Closing = self.Closing. a:def.Closing
endfunc
"}}}
function! s:PairMine.CanAppendNL(ctx) abort "{{{
  return a:ctx.CrrLine[a:ctx.Colx :] == '' && a:ctx.Row == self.Row + 1
endfunc
"}}}
function! s:PairMine.AppendNL(ctx) abort "{{{
  let idN = indent(self.Row)
  let space = &expandtab ? repeat(' ', idN) : repeat("\t", idN / &tabstop). repeat(' ', idN % &tabstop)
  call append(a:ctx.Row, space. self.Closing)
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
  let self.LeftLine = l < 0 ? '' : self.CrrLine[: l]

  let self.Trig = a:trig
  let self.TrigE = escape(a:trig, '\')
  let self.RightLine = self.CrrLine[a:colx :]
  let self.PreChar = matchstr(self.LeftLine, '.$')
  return self
endfunc
"}}}
function! s:Contexter.ApplyRegexpDef(def) abort "{{{
  let idx = match(self.LeftLine, '\V'. a:def.OpeningxPat. '\$')
  if idx==-1
    return 0
  end
  let a:def.Openingx = self.LeftLine[idx :]
  let a:def.Opening = a:def.Openingx. self.Trig
  let a:def.DefStr = a:def.Opening. a:def.Type!='pair' ? '' : ' '. a:def.Closing
  return 1
endfunc
"}}}
function! s:Contexter.FailInQuote(def) abort "{{{
  if self.RightLine =~# '^\V'. self.TrigE " 右隣に同じ字なく
    return 1
  elseif self.LeftLine !~# '\(^\|[^[:alnum:]_\\]\)\V'. escape(a:def.Openingx, '\'). '\$' " 左隣が英数など以外で始まり
    return 2
  elseif self.Colx!=0 && self.PreChar ==# self.Trig && a:def.Openingx !~# '\V'. self.TrigE. '\$' " 左隣に同じ字がある場合はOpeningxで定義されている
    return 3
  end
  return 0
endfunc
"}}}
function! s:Contexter.FailInPair(def) abort "{{{
  if self.RightLine =~# '^\w'
    return 1
  elseif self.LeftLine !~# (a:def.Opening =~# '^\w' ? '\(^\|[^[:alnum:]_]\)\V' : '\V'). escape(a:def.Openingx, '\'). '\$'
    return 2
  end
  return 0
endfunc
"}}}

function! autoclosing#init() abort "{{{
  let [s:chr2defs, s:chr2clldefs, s:cllTrgDefs, s:exclusionPats] = [{}, {}, [], []]
  let [s:_changedtick, s:batch_len] = [b:changedtick, 0]
  if !(v:insertmode=='i' && exists('g:autoclosing_def'))
    return
  end
  let s:inhibition_pats = s:filter_by_buftype_and(g:autoclosing_inhibition_pat, function('s:F_val'))
  let [condi2ds, excl_condi2ds, excl_condi2pat, s:chr2clldefs, s:cllTrgDefs] = s:parse_collecstr_n_split_remain(g:autoclosing_def)
  let [s:excl_ds, s:excl_pat] = map([excl_condi2ds, excl_condi2pat], 's:filter_by_buftype_and(v:val, function("s:F_nudeval"))')
  for [condi, ds] in s:filter_by_buftype_and(condi2ds, function('s:F_items'))
    let condis = split(condi, '|')
    for d in ds
      if s:matched_exclusion(d)
        continue
      end
      let is_imidiate = 0
      let idx = match(d, '\s\+$')
      if idx!=-1
        let is_imidiate = 1
        let d = d[: idx-1]
      end
      let sep = stridx(d, ' ')
      if sep == -1
        let ms = matchlist(d, '^\(.*\)\(.\)$')
        let trig = ms[2]
        let s:chr2defs[trig] = get(s:chr2defs, trig, []) + [{'Condis': condis, 'IsImidiate': is_imidiate, 'Type': 'quote', 'Openingx': ms[1], 'Opening': d}]
        continue
      end
      let [opening, closing] = [d[:sep-1], d[sep+1:]]
      let ms = matchlist(opening, '^\(.*\)\(.\)$')
      let trig = ms[2]
      let s:chr2defs[trig] = get(s:chr2defs, trig, []) + [{'Condis': condis, 'IsImidiate': is_imidiate, 'Type': 'pair', 'Openingx': ms[1], 'Opening': opening, 'Closing': closing}]
    endfor
  endfor
endfunc
"}}}
function! s:parse_collecstr_n_split_remain(autoclosing_def) abort "{{{
  let [l:condi2ds, l:excl_condi2ds, l:excl_condi2pat, l:chr2clldefs, l:cllTrgDefs] = [{}, {}, {}, {}, []]
  let MATCHSTRPOS = exists('*matchstrpos') ? function('matchstrpos') : function('s:matchstrpos')
  let PRINT_ITEM = '\%(\%(\%(^\|[^%]\)\%(%%\)*\)\@<=%\)\@<!%s'
  let LAST_IS_PRINTITEM = PRINT_ITEM. '$'
  let COLLEC_STR = '&&\[.\{-}\%(\%(\%(^\|[^\\]\)\%(\\\\\)*\)\@<=\\\)\@<!]'
  let COLLEC_CORE = '&&\zs\[.\{-}\%(\%(\%(^\|[^\\]\)\%(\\\\\)*\)\@<=\\\)\@<!]'
  let ret = []
  for [key, ds] in items(a:autoclosing_def)
    for d in ds
      if d !~ '&&['
        if d =~ '^\s'
          let l:excl_condi2ds[key] = get(l:excl_condi2ds, key, []) + [substitute(d, '^\s\+\|\s\+$', '', 'g')]
        else
          let l:condi2ds[key] = get(l:condi2ds, key, []) + [d]
        end
        continue
      end
      let fmt = substitute(substitute(d, '%', '%%', 'g'), COLLEC_STR, '%s', 'g')
      let is_excluded = 0
      let idx = matchend(fmt, '^\s\+')
      if idx!=-1
        let is_excluded = 1
        let fmt = fmt[idx :]
      end
      let is_imidiate = 0
      let idx = match(fmt, '\s\+$')
      if idx!=-1
        let is_imidiate = 1
        let fmt = fmt[: idx-1]
      end

      let collec_args = []
      let ms = MATCHSTRPOS(d, COLLEC_CORE, 0)
      while ms[1] != -1
        let collec_args += [ms[0]]
        let ms = MATCHSTRPOS(d, COLLEC_CORE, ms[2])
      endwhile

      if is_excluded
        let l:excl_condi2pat[key] = get(l:excl_condi2pat, key, []) + ['^\V'. call('printf', [escape(fmt, '\')] + collec_args). '\$']
        continue
      end

      let condis = split(key, '|')
      let sep = stridx(fmt, ' ')
      if sep == -1
        let idx = match(fmt, LAST_IS_PRINTITEM)
        if idx!=-1
          let fmtx = fmt[: idx-1]
          let trigpat = remove(collec_args, -1)
          let l:cllTrgDefs += [{'Condis': condis, 'IsImidiate': is_imidiate, 'Type': 'quote', 'TrigPat': trigpat, 'OpeningxPat': call('printf', [escape(fmtx, '\')] + collec_args)}]
        else
          let ms = matchlist(fmt, '^\(.*\)\(.\)$')
          let [ofmtx, trig] = [ms[1], ms[2]]
          let chr2clldefs[trig] = get(chr2clldefs, trig, []) + [{'Condis': condis, 'IsImidiate': is_imidiate, 'Type': 'quote', 'OpeningxPat': call('printf', [escape(ofmtx, '\')] + collec_args)}]
        end
        continue
      end

      let [ofmt, cfmt] = [fmt[:sep-1], fmt[sep+1:]]
      if cfmt =~ PRINT_ITEM
        call autocloser#__warning('invalid definition `'. d. '`: cannot use "&&[]" in closing pattern.')
        continue
      end
      let idx = match(ofmt, LAST_IS_PRINTITEM)
      if idx!=-1
        let ofmtx = ofmt[: idx-1]
        let l:cllTrgDefs += [{'Condis': condis, 'IsImidiate': is_imidiate, 'Type': 'pair', 'TrigPat': collec_args[-1],
          \ 'Closing': cfmt, 'OpeningxPat': len(collec_args)==1 ? ofmtx : call('printf', [escape(ofmtx, '\')] + collec_args[-2])}]
        continue
      end
      let ms = matchlist(ofmt, '^\(.*\)\(.\)$')
      let [ofmtx, trig] = [ms[1], ms[2]]
      let l:chr2clldefs[trig] = get(l:chr2clldefs, trig, []) + [{'Condis': condis, 'IsImidiate': is_imidiate, 'Type': 'pair',
        \ 'Closing': cfmt, 'OpeningxPat': call('printf', [escape(ofmtx, '\')] + collec_args)}]
    endfor
  endfor
  return [condi2ds, excl_condi2ds, excl_condi2pat, chr2clldefs, cllTrgDefs]
endfunc
"}}}
function! autoclosing#insert_pre() abort "{{{
  if !s:during_feedkeys
    let s:batch_len += 1
  end
endfunc
"}}}
function! autoclosing#cleanup() abort "{{{
  call s:Leavablee.Reset()
  let [s:mines, s:almostvalid, s:within_butt, s:gone_butt, s:during_feedkeys, s:feedkeys, s:save_row, s:save_colx] = [[], {}, {}, {}, 0, '', 0, 0]
  unlet! s:chr2defs s:chr2clldefs s:cllTrgDefs s:exclusionPats s:excl_ds s:excl_pat
  unlet! s:inhibition_pats s:_changedtick s:batch_len
endfunc
"}}}

let [s:during_feedkeys, s:save_row, s:save_colx, s:feedkeys] = [0, 0, 0, '']
function! autoclosing#chk_et_append() abort "{{{
  if !exists('s:inhibition_pats')
    return
  end
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
      let s:mines += [def.Type==#'quote' ? s:newQuoteMine(def, ctxer) : s:newPairMine(def, ctxer)]
    end
    let i += 1
  endwhile
  let c = targchars[-1]
  let def = s:obtain_def(ctxer.About(c, a:ctx.Colx))
  let result = s:or_chain(ctxer, def, feeds,
    \ function('s:complement_premine_closing'), function('s:resemble_premine'), function('s:handle_inserted_1char_n_closing'), function('s:set_mine'), function('s:sep_inputted_then_turn'))
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
  let [s:within_butt, s:gone_butt] = [{}, {}]
  if !a:is_changed
    call s:Leavablee.Reset()
  elseif s:mines!=[] && s:mines[0].Row != a:ctx.Row
    call s:Leavablee.Reset()
    if s:mines[0].CanAppendNL(a:ctx)
      call s:mines[0].AppendNL(a:ctx)
    end
  end
  let s:mines = []
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
  if s:mines==[] || a:ctxer.RightLine !~# '^\V'. escape(s:mines[-1].Closing, '\')
    return 0
  end
  let mine = remove(s:mines, -1)
  return strchars(mine.Closing)
endfunc
"}}}
function! s:complement_premine_closing(ctxer, _, feeds) abort "{{{
  if s:mines==[] || matchstr(s:mines[-1].Closing, '^.') !=# a:ctxer.Trig
    return ''
  end
  let mine = remove(s:mines, -1)
  call add(a:feeds , substitute(mine.Closing, '^.', '', ''))
  let [s:gone_butt, s:almostvalid] = [mine, mine]
  return 'handle_premine: complemented closing'
endfunc
"}}}
function! s:resemble_premine(ctxer, def, _) abort "{{{
  if s:mines==[] || a:def=={}
    return ''
  end
  let mine = s:mines[-1]
  if get(mine, 'OrgOpening', '') ==# a:def.Opening " 同じOpeningを持つなら重ねる
    call mine.Accum(a:def, a:ctxer)
    return 'handle_premine: accumulated'
  elseif matchstr(mine.Opening, '^.') ==# matchstr(a:def.Opening, '^.') " 開始文字が同じ場合、復活の可能性
    let s:almostvalid = remove(s:mines, -1)
  end
  return ''
endfunc
"}}}
function! s:handle_inserted_1char_n_closing(ctxer, _, feeds) abort "{{{
  if s:within_butt=={} || !(matchstr(s:within_butt.Closing, '^.') ==# a:ctxer.Trig && a:ctxer.RightLine =~# '^\V'. escape(s:within_butt.Closing, '\'))
    return ''
  end
  if s:mines!=[] && s:mines[-1].Colx == a:ctxer.Colx-1 " '`"`' などの入れ子になったトリガ対策
    call remove(s:mines, -1)
  end
  call add(a:feeds , "\<C-g>U\<BS>". repeat("\<C-g>U\<Right>", strchars(s:within_butt.Closing)))
  let s:within_butt = {}
  return 'handle_inserted_1char_n_closing'
endfunc
"}}}
function! s:set_mine(ctxer, def, feeds) abort "{{{
  if a:def=={}
    return ''
  end
  let s:mines += [a:def.Type==#'quote' ? s:newQuoteMine(a:def, a:ctxer) : s:newPairMine(a:def, a:ctxer)]
  return 'set_mine'
endfunc
"}}}
function! s:sep_inputted_then_turn(ctxer, _, feeds) abort "{{{
  if s:gone_butt=={} || !g:autoclosing_enable_separator_turnback
    return ''
  elseif !(index(s:EMPBACK_SEPS, a:ctxer.Trig)!=-1 && s:gone_butt.IsValidPos(a:ctxer) && a:ctxer.LeftLine =~# '\V'. escape(s:gone_butt.Opening. s:gone_butt.Closing, '\'). '\$')
    let s:gone_butt = {}
    return ''
  end
  let str = s:gone_butt.Closing. a:ctxer.Trig
  call add(a:feeds , repeat("\<C-g>U\<Left>", strchars(str)))
  call s:Leavablee.Reset().Store(str, a:ctxer)
  let s:gone_butt = {}
  return 'sep_inputted_then_turn'
endfunc
"}}}
function! s:ignite_mines(ctxer, feeds) abort "{{{
  for pat in s:inhibition_pats
    if search(pat, 'bcWn')
      call filter(s:mines, '!v:val.IsImidiate && v:val.Colx == a:ctxer.Colx')
      return
    end
  endfor
  let [lenM, is_first] = [len(s:mines), 1]
  while s:mines!=[] && (s:mines[0].Colx != a:ctxer.Colx || s:mines[0].IsImidiate)
    let mine = remove(s:mines, 0)
    if !mine.IsValidPos(a:ctxer)
    elseif !(mine.IsIgnitable(a:ctxer) || lenM > 1 && is_first)
      let s:almostvalid = mine
    else
      call add(a:feeds, mine.Closing. repeat("\<C-g>U\<Left>", strchars(mine.Closing)))
      let s:within_butt = mine
      call s:Leavablee.Store(mine.Closing, a:ctxer)
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
function! s:filter_by_buftype_and(hash, AppendF) abort "{{{
  let ret = []
  let [fts, bext] = [split(&ft, '\.'), expand('%:e')]
  for item in items(a:hash)
    for condi in split(item[0], '|')
      if ((condi =~ '^.' && bext ==? condi[1:]) || match(fts, substitute(condi, '\*', '.\\+', 'g'))!=-1)
        let ret += a:AppendF(item)
        break
      end
    endfor
  endfor
  return ret
endfunc
"}}}
function! s:F_nudeval(items) abort "{{{
  return a:items[1]
endfunc
"}}}
function! s:F_val(items) abort "{{{
  return [a:items[1]]
endfunc
"}}}
function! s:F_items(items) abort "{{{
  return [a:items]
endfunc
"}}}
function! s:matched_exclusion(defstr) abort "{{{
  if index(s:excl_ds, a:defstr)!=-1
    return 1
  end
  for pat in s:excl_pat
    if a:defstr =~# pat
      return 1
    end
  endfor
  return 0
endfunc
"}}}
function! s:obtain_def(ctxer) abort "{{{
  let trig = a:ctxer.Trig
  let defs1 = get(s:chr2defs, trig, [])
  let defs2 = filter(get(s:chr2clldefs, trig, [])[:], 'a:ctxer.ApplyRegexpDef(v:val) && !s:matched_exclusion(v:val.DefStr)')
  let defs3 = filter(s:cllTrgDefs[:], 'trig =~# v:val.TrigPat && a:ctxer.ApplyRegexpDef(v:val) && !s:matched_exclusion(v:val.DefStr)')
  if defs1==[] && defs2==[] && defs3==[]
    return {}
  end
  let acc = []
  let [fts, bext] = [split(&ft, '\.'), expand('%:e')]
  for def in defs1 + defs2 + defs3
    for condi in def.Condis
      if ((condi =~ '^.' && bext ==? condi[1:]) || match(fts, substitute(condi, '\*', '.\\+', 'g'))!=-1)
        if !a:ctxer[def.Type=="quote" ? 'FailInQuote' : 'FailInPair'](def)
          let acc += [{'strchars': strchars(def.Openingx), 'condi': condi, 'type': def.Type, 'def': def}]
          break
        end
      end
    endfor
  endfor
  if acc==[]
    return {}
  end
  call sort(acc, 's:_sort_df')
  return acc[-1].def
endfunc
"}}}
function! s:_sort_df(a, b) abort "{{{
  if a.condi != b.condi
    let ret = a.condi=='*' ? -1 : b.condi=='*' ? 1 : 0
    if ret
      return ret
    end
  end
  let ret = a.strchars - b.strchars
  return ret || a.Type==#b.Type ? ret : a.Type=='pair' ? -1 : 1
endfunc
"}}}
function! s:matchstrpos(str, pat, start) abort "{{{
  let [bgn, end] = [match(a:str, a:pat, a:start), matchend(a:str, a:pat, a:start)]
  return [a:str[bgn : end-1], bgn, end]
endfunc
"}}}
function! autoclosing#__warning(msg) abort "{{{
  echoh Error
  echom "autocloser.vim: ". a:msg
  echoh NONE
endfunc
"}}}
"=============================================================================
"END "{{{1
let &cpo = s:save_cpo| unlet s:save_cpo
