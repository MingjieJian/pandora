;;
;; this is a simple example to read a .out table, using read_out()
;; get the heading, and plot the profile(s)
;;
;; the prof_N-M files may have I/A at various Mu, then F/A
;;
;; -> I/A for 6 vals of Mu, request to save 2 lines of header
;;    F/A is the 7th table
;;
xCol = 'DL'
yCol = 'F/A'
fn = 'SPh.prof_5-1.out.001'
;;
nCols = 5
nHdr = 14
nSkip = 8
nTables = 1
;;
t = read_out(fn, nTables = nTables, nSkip = nSkip, $
             nHdr = nHdr, nCols = nCols, verbose = 0, no_on = 0)
;;
dta = *t._dta[0]
lbl = *t._lbl[0]
hdr = *t._hdr[0]
;;
print, t.model
print, t.type
print, hdr
;;
i = where(lbl EQ xCol, cnt)
IF cnt NE 1 THEN message,  'no '+xCol+' in '+fn
j = where(lbl EQ yCol, cnt)
IF cnt NE 1 THEN message,  'no '+yCol+' in '+fn
i = i[0]
j = j[0]
;;
plot, dta[i, *], dta[j, *], $
  xrange = [-1, 1]*0.85, /xstyle, $
  xtitle = lbl[i], ytitle = lbl[j], title = fn
;;
rxyouts, .05, .95, t.model
rxyouts, .05, .90, t.type
;;
!p.multi = 0
END
