;;
;; this script compares two cases.
;;
;;  extract 'MODEL DATA' leidh.aaa.000 leidh.model_data.out.001
;;  extract 'PROF (3/2)' leidh.aaa.000 leidh.prof_3-2.out.001
;;
;;  extract 'MODEL DATA' leidh.aaa.001 leidh.model_data.out.002
;;  extract 'PROF (3/2)' leidh.aaa.001 leidh.prof_3-2.out.002
;;
;; <- Last updated: Fri Mar 21 14:53:19 2014 -> SGK
;;
PRO p2plot, t1, t2, fnp1, fnp2, residual = residual
;;
residual = keyword_set(residual)
;;
dta1 = *t1._dta[0]              ; this holds the data (prof 3/2)
hdr1 = *t1._hdr[0]              ; this hold the header
lbl1 = *t1._lbl[0]              ; this hold the table labels
;;
dta2 = *t2._dta[0]              ; this holds the data
hdr2 = *t2._hdr[0]              ; this hold the header
lbl2 = *t2._lbl[0]              ; this hold the table labels
;;
tmod  = t1.model
ptyp1 = t1.type
ptyp2 = t2.type
;;
xCol = 'DL'
IF residual THEN yCol = 'Residual' ELSE yCol = 'F/A'
;;
i = where(lbl1 EQ xCol, cnt)
IF cnt NE 1 THEN message,  'no '+xCol+' in '+fn
j = where(lbl1 EQ yCol, cnt)
IF cnt NE 1 THEN message,  'no '+yCol+' in '+fn
i = i[0]
j = j[0]
;;
!x.range = [-1., 1]*3
plot,  dta1[i, *], dta1[j, *], xtitle = lbl1[i], ytitle = lbl1[j], /xstyle, psym = -1, $
  title = ptyp1
oplot, dta2[i, *], dta2[j, *], psym = -4, color = 1
;;
rxyouts, .05, .05, '+: '+fnp1
rxyouts, .05, .10, 'o: '+fnp2, color = 1
;;
IF residual THEN s = 1e-2 ELSE s = max(dta1[j, *])/100
;;
plot, dta1[i, *], (dta1[j, *]-dta2[j, *])/s, xtitle = lbl1[i], /xstyle, $
  title = fnp1+'-'+fnp2, ytitle = 'Difference of '+lbl1[j]+' [in % wrt continuum]'
!x.range = 0
END
;;
;; ---------------------------------------------------------------------------
;;
IF n_elements(xfn1) EQ 0 THEN BEGIN
;;
  xfn1 = '.001'
  xfn2 = '.002'
  ;;
  ;; read the two models
  ;;
  fnm1 = 'leidh.model_data.out'+xfn1
  fnm2 = 'leidh.model_data.out'+xfn2
  ;;
  nHdr    = 6                   ; save 6 header lines
  nTables = 1                   ; read only 1 table
  nSkip   = 0                   ; don't skip any
  nL4LH   = 4                   ; column heading (labels) are on 4 lines
  m1 = read_out(fnm1, nTables = nTables, nSkip = nSkip, $
                nHdr = nHdr, nCols = nCols, nL4LH = nL4LH, verbose = 0, /no_onerr)
  m2 = read_out(fnm2, nTables = nTables, nSkip = nSkip, $
                nHdr = nHdr, nCols = nCols, nL4LH = nL4LH, verbose = 0)
  ;;
  ;; read the two profiles
  ;;
  ;; H 3/2
  fnp1h = 'leidh.prof_3-2.out'+xfn1
  fnp2h = 'leidh.prof_3-2.out'+xfn2
  ;;
  ;; F/A is the 7th table
  ;;
  nHdr    = 2                   ; save 2 header lines
  nTables = 1                   ; read only 1 table
  nSkip   = 6                   ; skip 6 tables (mu = )
  nL4LH   = 0                   ; colunm heading are not on multiple lines
  ;;
  t1h = read_out(fnp1h, nTables = nTables, nSkip = nSkip, $
                nHdr = nHdr, nCols = nCols, verbose = 0)
  t2h = read_out(fnp2h, nTables = nTables, nSkip = nSkip, $
                nHdr = nHdr, nCols = nCols, verbose = 0)
  ;;
  ;; HeI 4/2
  fnp1he1 = 'leidhe1.prof_4-2.out'+xfn1
  fnp2he1 = 'leidhe1.prof_4-2.out'+xfn2
  ;;
  ;; F/A is the 7th table
  ;;
  nHdr    = 2                   ; save 2 header lines
  nTables = 1                   ; read only 1 table
  nSkip   = 6                   ; skip 6 tables (mu = )
  nL4LH   = 0                   ; colunm heading are not on multiple lines
  ;;
  t1he1 = read_out(fnp1he1, nTables = nTables, nSkip = nSkip, $
                nHdr = nHdr, nCols = nCols, verbose = 0)
  t2he1 = read_out(fnp2he1, nTables = nTables, nSkip = nSkip, $
                nHdr = nHdr, nCols = nCols, verbose = 0)
  ;;
  ;; CaII 5/1
  fnp1ca2 = 'leidca2.prof_5-1.out'+xfn1
  fnp2ca2 = 'leidca2.prof_5-1.out'+xfn2
  ;;
  ;; F/A is the 7th table
  ;;
  nHdr    = 2                   ; save 2 header lines
  nTables = 1                   ; read only 1 table
  nSkip   = 0                   ; skip 0 tables (no mu = )
  nL4LH   = 0                   ; colunm heading are not on multiple lines
  ;;
  t1ca2 = read_out(fnp1ca2, nTables = nTables, nSkip = nSkip, $
                nHdr = nHdr, nCols = nCols, verbose = 0)
  t2ca2 = read_out(fnp2ca2, nTables = nTables, nSkip = nSkip, $
                nHdr = nHdr, nCols = nCols, verbose = 0)
  ;;
  bwin, /plct
  ;;
ENDIF
;;
;; not plot all this
;;
!p.multi = [0, 1, 3]
IF !d.name EQ 'X' THEN !p.charsize = 2
;;
;; first the model
;;
dta1 = *m1._dta[0]              ; this holds the data (model)
dta2 = *m2._dta[0]              ; this holds the data
lbl  = *m1._lbl[0]              ; this hold the table labels (column headings)
mmod1 = m1.model                ; hold the data model
mmod2 = m2.model                ; hold the data model
mtyp1 = m1.type                 ; hold the data type
mtyp2 = m2.type                 ; hold the data type
;;
z1 = dta1[0, *]                 ; column 1 is the depth
z2 = dta2[0, *]
;;
j = 2                           ; column 2 is the temp
;;
plot,  z1, dta1[j, *], xtitle = lbl[0], ytitle = lbl[j], /xstyle, psym = -1, ylog = ylog, title = fnm1+' vs '+fnm2
oplot, z2, dta2[j, *], psym = -4, color = 1
;;
rxyouts, .07, .05, mmod1
rxyouts, .07, .10, '+: '+fnm1
rxyouts, .07, .15, 'o: '+fnm2, color = 1
;;
;; list which columns to plot (room for 6)
jList = [1, 5, 6, 7, 8, 9]      ;, 10]
yLogs = [1, 1, 1, 1, 1, 1]      ;, 1] ; use 1 to plot log(y) (/ylog)
;;
!p.multi = [6, 3, 3]
FOR k = 0, n_elements(jList)-1 DO BEGIN 
  j    = jlist[k]
  ylog = ylogs[k]
  ; print, 'plotting '+lbl[j]
  plot,  z1, dta1[j, *], xtitle = lbl[0], ytitle = lbl[j], /xstyle, psym = -1, ylog = ylog
  oplot, z2, dta2[j, *], psym = -4, color = 1
ENDFOR
;;
answ = ''
read, 'CR> ', answ
;;
;; now the profiles
;;
!p.multi = [0, 2, 3]
residual = 1                    ; 0
p2plot, t1h,   t2h,   fnp1h,   fnp2h,   residual = residual
p2plot, t1he1, t2he1, fnp1he1, fnp2he1, residual = residual
p2plot, t1ca2, t2ca2, fnp1ca2, fnp2ca2, residual = residual

;;
!p.multi = 0
!p.charsize = 0
END
