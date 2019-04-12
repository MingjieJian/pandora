;;
;; this is a simple example to read a .out table, using read_out()
;; get the heading, and plot the profile(s)
;;
;; the prof_N-M files may have I/A at various Mu, then F/A
;;
;; -> I/A for 6 vals of Mu, request to save 2 lines of header
;;    F/A is the 7th table
;;
fn = 'leidh.prof_3-2.out.001'   & nMus = 6
fn = 'leidhe1.prof_4-2.out.001' & nMus = 6
fn = 'leidca2.prof_5-1.out.001' & nMus = 0
;;
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
  !p.multi = [0, 1, 2]
  ;;
  plot,   dta[i, *], dta[j, *], line = 1, $
    xtitle = lbl[i], ytitle = lbl[j], title = fn, /xstyle
  xyouts, dta[i, 0], dta[j, 0]*1.01, '   mu='+mu
  ;;
  FOR k = 1, nMus-1 DO BEGIN
    dta = *t._dta[k]            ; this holds the data
    hdr = *t._hdr[k]            ; this hold the header - 2 lines
    mLine = hdr[0]              ; this line holds the value of Mu
    mu = (strsplit(mLine, ' ', /extract))[4] ; the 5th word holds the value of Mu (; --- Mu = 0.___ ---)
    oplot,  dta[i, *], dta[j, *], line = (k MOD 2) +1
    xyouts, dta[i, 0], dta[j, 0]*1.01, '   mu='+mu
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
plot, dta[i, *], dta[j, *], xtitle = lbl[i], ytitle = lbl[j], title = fn, /xstyle
;;
!p.multi = 0
END
