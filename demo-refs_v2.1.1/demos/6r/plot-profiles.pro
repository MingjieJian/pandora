;;
;; like plot-profile, but plot several
;;
PRO pprof, fn, nMus, $
           xrange = xrange, ymurange = ymurange, yrange = yrange
;;
xCol = 'DL'
yCol = 'I/A'
;;
nHdr = 2
nTables = nMus+1                ; 7
;;
t = read_out(fn, nTables = nTables, nSkip = nSkip, $
             nHdr = nHdr, nCols = nCols, verbose = 0, no_on = 0)
;;
IF nMus GT 0 THEN BEGIN 
  ;;
  dta = *t._dta[0]              ; this holds the data
  hdr = *t._hdr[0]              ; this hold the header - 2 lines
  lbl = *t._lbl[0]              ; this hold the table labels
  ;;
  i = where(lbl EQ xCol, cnt)
  IF cnt NE 1 THEN message,  'no '+xCol+' in '+fn
  j = where(lbl EQ yCol, cnt)
  IF cnt NE 1 THEN message,  'no '+yCol+' in '+fn
   i = i[0]
  j = j[0]
  ;;
  mLine = hdr[0]                         ; this line holds the value of Mu
  mu = (strsplit(mLine, ' ', /extract))[4] ; the 5th word holds the value of Mu (; --- Mu = 0.___ ---)
  ;;
  plot,   dta[i, *], dta[j, *], line = 1, $
    xtitle = lbl[i], ytitle = lbl[j], title = fn, /xstyle, $
    xrange = xrange, yrange = ymurange
  xyouts, !x.crange[0], dta[j, 0]*1.01, '   mu='+mu
  ;;
  FOR k = 1, nMus-1 DO BEGIN
    dta = *t._dta[k]            ; this holds the data
    hdr = *t._hdr[k]            ; this hold the header - 2 lines
    mLine = hdr[0]              ; this line holds the value of Mu
    mu = (strsplit(mLine, ' ', /extract))[4] ; the 5th word holds the value of Mu (; --- Mu = 0.___ ---)
    oplot,  dta[i, *], dta[j, *], line = (k MOD 2) +1
    xyouts, !x.crange[0], dta[j, 0]*1.01, '   mu='+mu
  ENDFOR 
;;
ENDIF
;;
dta = *t._dta[nMus]
lbl = *t._lbl[nMus]
;;
xCol = 'DL'
yCol = 'F/A'
i = where(lbl EQ xCol, cnt)
IF cnt NE 1 THEN message,  'no '+xCol+' in '+fn
j = where(lbl EQ yCol, cnt)
IF cnt NE 1 THEN message,  'no '+yCol+' in '+fn
i = i[0]
j = j[0]
;;
plot, dta[i, *], dta[j, *], xtitle = lbl[i], ytitle = lbl[j], $
  title = fn, /xstyle, xrange = xrange, yrange = yrange
;;
END
;;
;;
;;
bwin
!p.multi = [0, 3, 2, 0, 1]
IF !d.name EQ 'X' THEN !p.charsize = 2.5
;;
pprof, 'leidh.prof_3-2.out.001',   6, xrange = [-1, 1]*7, yrange = yrange
pprof, 'leidhe1.prof_4-2.out.001', 6, xrange = [-1, 1]*7, yrange = [.9, 1]*1.9e6
;;
!p.multi = [1, 3, 2, 0, 1]
pprof, 'leidca2.prof_5-1.out.001', 0, xrange = xrange, yrange = yrange
;;
!p.multi = 0
!p.charsize = 0
END
