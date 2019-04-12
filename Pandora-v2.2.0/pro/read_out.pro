FUNCTION itoa, i
return, string(i, format = '(i0)')
END
;;
FUNCTION strtr, line, strIn, strOut
;;
;; translatea chars in strIn to chars in strOut
;;   new = strtr(line,'()', ' ') --> replace '(' or '(' by ' '
;;
c_line = byte(line)
c_in   = byte(strIn)
c_out  = byte(strOut)
;;
IF n_elements(c_out) NE 1 THEN message, ' tanslates only to one char, not'+itoa(n_elements(c_out))
;;
FOR i = 0, n_elements(c_line)-1 DO BEGIN
  FOR k = 0, n_elements(c_in)-1 DO BEGIN 
    IF c_line[i] EQ c_in[k] THEN c_line[i] = c_out[0]
  ENDFOR 
ENDFOR 
return, string(c_line)
END
;;
FUNCTION read_out, fn, $
                   nTables = nTables, nSkip = nSkip, $
                   nHdrs = nHdrsIn, nCols = nColsIn, $
                   useWords = useWords, nL4LH = nL4LH, $
                   nLines = nLines, verbose = verbose, $
                   no_onerror = no_onerror
;;
;; read a *.out file, assuming it was converted to a "machine readable format"
;; namely
;;   the very 1st line is ';    > model name'
;;   the      2nd line is '1PNS... type name'
;;   then each line is either
;;    1st char is ';' -> comment that may be saved as header
;;    else line if made of cols of numbers, 
;;      but ignore the 1st column (index)
;;      the 1st data line is parsed to delineate each column (assume fixed widths)
;;      /useWord overrides this and each data line is broken in space-separated words (asssume constant number of words)
;;
;; when prev line is ^; and current line is not:
;;   save header (nHdrs previous comment lines)
;;   start a new table
;;
;; return a structure:
;;   s.name - string              - model name
;;   s.type - string              - out file type
;;   s._hdr[*] - ptrArr to strArr - header
;;   s._lbl[*] - ptrArr to strArr - labels for each column
;;   s._dta[*] - ptrArr to dblArr - actual data
;;
;; by default, the # of cols in the table is set by the number of words in 1st line
;; but, one can speficy # cols
;;
;; parse as many tables as available, or only nTables if supplied
;;
;;  nTables: read only that many tables leave 0 to read all
;;  nSkip:   skip that many tables
;;  nHdrs:   no of comment lines before a table to save as a header
;;  nCols:   read only that many colunm for the table(s)
;;    if nCols and nHdrs are scalar, apply to all tables
;;                       are vectors,  size() == nTables
;;  nL4LH:      number of lines for label headings, def = 1 (scalar)
;;  useWords:   if set, use strsplit() at each data line
;;  nLines:     hint to length of lines (def 200) but table array will grows as needed
;;  verbose:    verbosity level (0-3)
;;  no_onerror: if set, disable the 'on_error, 2'
;;
;; <- Last updated: Fri Nov  1 15:44:54 2013 -> SGK
;;
;; ---------------------------------------------------------------------------
;; NYI:
;; alternate specification: 
;;  arrays of strings in the form [name]@N[.M]
;;      where N: colum# M: number of columns, 
;;      ie 'DL@10.8', 'I@20.8' means variable DL is 8 chars at column 10, 
;;      variable I  is 8 chars at column 20
;;
IF NOT keyword_set(no_onerror) THEN on_error, 2
;;
IF NOT keyword_set(nTables) THEN nTables = 0
IF NOT keyword_set(nSkip)   THEN nSkip   = 0
IF NOT keyword_set(nLines)  THEN nLines  = 300
IF NOT keyword_set(nColsIn) THEN nColsIn = 0
IF NOT keyword_set(nHdrsIn) THEN nHdrsIn = 1
IF NOT keyword_set(nL4LH)   THEN nL4LH   = 1
IF NOT keyword_set(verbose) THEN verbose = 0
useWords = keyword_set(useWords)
;;
IF nTables LT 0 THEN message, 'invalid nTables: ' + itoa(nTables)
IF nSkip   LT 0 THEN message, 'invalid nSkip: '   + itoa(nSkip)
;;
IF min(nColsIn) LT 0 THEN message, 'invalid nCols: ' + itoa(nColsIn)
IF min(nHdrsIn) LT 0 THEN message, 'invalid nHdrs: ' + itoa(nHdrsIn)
;;
IF nTables GT 0 THEN BEGIN 
  IF n_elements(nColsIn) EQ 1 THEN nCols = replicate(nColsIn, nTables)
  IF n_elements(nHdrsIn) EQ 1 THEN nHdrs = replicate(nHdrsIn, nTables)
  IF n_elements(nCols) NE nTables THEN  message, 'invalid nCols: #val is only '+itoa(n_elements(nCols))
  IF n_elements(nHdrs) NE nTables THEN  message, 'invalid nHdrs: #val is only '+itoa(n_elements(nHdrs))
ENDIF ELSE BEGIN
  nCols = nColsIn
  nHdrs  = nHdrsIn
ENDELSE
;;
nHdrsMax = max(nHdrs)
pLines = strarr(nHdrsMax)
ipL    = 0
;;
IF nTables EQ 0 THEN nPtr = 10 ELSE nPtr = nTables
ptr2hdr = ptrarr(nPtr)
ptr2lbl = ptrarr(nPtr)
ptr2dta = ptrarr(nPtr)
iSet  = 0
prevC = ''
line  = ''
openr, lu, fn, /get_Lun
n = 0L
;;
s = {model:'', type:'', nTables:0L}
loop:
readf, lu, line
n++
;;
CASE n OF
  ;;
  1: BEGIN
    i = strPos(line, '>')
    IF i EQ -1 THEN message, 'input file is not as expected'   
    s.model = strMid(line, i+2, 199)
  END
  ;;
  2: BEGIN
    s.type = strMid(line, 13, 199)
  END
  ;;
  ELSE: BEGIN 
    c = strMid(line, 0, 1)
    IF c EQ ';' THEN BEGIN
      ;;
      ;; comment line to ignore
      ;;
      IF prevc NE c AND prevc NE '' THEN BEGIN
        ;;
        ;; 1st comment line after a table
        ;; save the header/table we just loaded
        ;;
        IF iSet GE nSkip THEN BEGIN
          IF verbose GT 1 THEN print, 'set ', iSet, hdr
          ;;
          IF iSet EQ nPtr THEN BEGIN
            q = ptrarr(nPtr+10)
            q[0:nPtr-1] = ptr2hdr
            ptr2hdr = q
            ;;
            q = ptrarr(nPtr+10)
            q[0:nPtr-1] = ptr2lbl
            ptr2lbl = q
            ;;            
            q = ptrarr(nPtr+10)
            q[0:nPtr-1] = ptr2dta
            ptr2dta = q
            ;;
            nPtr += 10
            ;;
            q = 0
          ENDIF
          ;;          
          ptr2hdr[iSet-nSkip] = ptr_new(hdr)
          ptr2lbl[iSet-nSkip] = ptr_new(lbl)
          ptr2dta[iSet-nSkip] = ptr_new(dta[*, 0:i-1])
        ENDIF ELSE BEGIN
          IF verbose GT 0 THEN  message, /info, 'skipping data table #'+itoa(iSet+1)+', nSkip='+itoa(nskip)
        ENDELSE
        iSet++
        ;;
      ENDIF 
      ;;
      IF verbose GT 2 THEN print, line
      ;;
    ENDIF ELSE BEGIN
      ;;
      ;; data line
      ;;
      IF prevc NE c THEN BEGIN
        ;; start of a new table
        i = 0L
        IF iSet GE nTables+nSkip AND nTables GT 0 THEN GOTO, doneReading
      ENDIF
      ;;
      IF iSet GE nSkip THEN BEGIN
        ;;
        IF i EQ 0 THEN BEGIN
          ;;
          IF nTables EQ 0 THEN BEGIN
            nColX = nCols
            nHdrX = nHdrs
          ENDIF ELSE BEGIN
            nColX = nCols[iSet-nSkip]
            nHdrX = nHdrs[iSet-nSkip]
          ENDELSE 
          ;;
          IF useWords THEN BEGIN
            ;;
            idw = strsplit(line, ' ')
            ;;
            IF nColX EQ 0 THEN nColX = n_elements(idw)-1
          ENDIF ELSE begin
            ;;
            ;; the cols start at prev col+len, since Rudy's adaptive-F format if not filling like a E format
            idc = strsplit(line, ' ', length = len)
            idc = idc+len
            ;;
            IF nColX EQ 0 THEN nColX = n_elements(idc)-1
          ENDELSE
          ;;
          IF nHdrX EQ 1 THEN hdr = pLines[ipL] ELSE BEGIN
            hdr = strarr(nHdrX)
            FOR j = -nHdrX+1, 0 DO $
              hdr[nHdrX-1+j] = pLines[((ipL+j+nHdrX) MOD nHdrX)]
          ENDELSE
          ;;
          dta = dblarr(nColX, nLines)
          ;;
          ;; the column labels are sometimes multi-lines
          IF nL4LH EQ 1 THEN BEGIN
            IF useWords THEN BEGIN
              lbl = pLines[ipL]
              lbl = strtr(lbl, '()', ' ') ; replace ') or '(' by ' '
              lbl = (strsplit(lbl, ' ', /extract))[1:nColX] ;  break the table headings, ignore 1st word
            ENDIF ELSE BEGIN
              lbl = strarr(nColX)
              lhdr = pLines[ipL]
              idx = [idc, strlen(lhdr)]
              FOR k = 0, nColX-1 DO BEGIN
                w = strtrim(strmid(lhdr, idx[k]+1, idx[k+1]-idx[k]), 2)
                lbl[k] = strtrim(w, 2)
              ENDFOR
            ENDELSE
          ENDIF ELSE BEGIN
            lbl = strarr(nColX)
            FOR j = -nL4LH+1, 0 DO BEGIN
              lhdr = pLines[( (ipL+j+nHdrX) MOD nHdrX)]
              idx = [idc, strlen(lhdr)]
              FOR k = 0, nColX-1 DO BEGIN
                w = strtrim(strmid(lhdr, idx[k]+1, idx[k+1]-idx[k]), 2)
                lbl[k] = lbl[k]+ ' '+w
              ENDFOR
            ENDFOR
            FOR k = 0, nColX-1 DO BEGIN
              lbl[k] = strtrim(lbl[k], 2)
           ENDFOR
          ENDELSE
        ENDIF 
        ;;
        IF i GE nLines THEN BEGIN
          sz = size(d)
          nLines =+ sz[2]
          dd = dblarr(nColX, nLines)
          dta  = dd
          dd = 0
        ENDIF
        ;;
        IF useWords THEN BEGIN
          w  = strsplit(line, ' ', /extract)
          nw = n_elements(w)
          IF nw LT nColX THEN  message, 'invalid data table at line n='+itoa(n)+$
            ' - found only '+itoa(nw)+' columns (words) instead of '+itoa(nColX)+$
            ' (file='+fn+')'
          dta[*, i] = double(w[1:nColX])
        ENDIF ELSE BEGIN
          idx = [idc, strlen(line)]
          FOR k = 0, nColX-1 DO BEGIN 
            w = strtrim(strmid(line, idx[k], idx[k+1]-idx[k]), 2)
            dta[k, i] = double(w)
          ENDFOR
        ENDELSE
        i++
      ENDIF
    ENDELSE
    prevc = c
  END
ENDCASE
ipL++
ipL =  ipL MOD nHdrsMax
pLines[ipL] = line
;;
IF NOT eof(lu) THEN GOTO, loop
;;
doneReading:
close, lu
free_lun, lu
;;
;; do I need to save the last table?
;;
iSet = iSet - nSkip
IF iSet LE 0 THEN  message, 'no tables found. (nTables='+itoa(nTables)+' nSkip='+itoa(nSkip)+')'
s.nTables = iSet
ptr2hdr = ptr2hdr[0:iSet-1]
ptr2lbl = ptr2lbl[0:iSet-1]
ptr2dta = ptr2dta[0:iSet-1]
s = create_struct(s, '_hdr', ptr2hdr, '_lbl',  ptr2lbl, '_dta', ptr2dta)
return, s
END
