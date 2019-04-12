FUNCTION strSep, string, separator, count = count, _EXTRA = extra
;
;+
;
; list = strSep(string, separator [,count = count])
;
; calls str_sep, and returns only the non-empty fields, uses ' ' if separator
; argument is missing
;
; Output Keyword:
;
;  count: no of elements of the list
;
;-
;  --------------- -
;= STRSEP          - calls strsplit(string, separator, /extract)
; ---------------------------------------------------------------------------
;  STRSEP          - calls str_sep, and returns only the non-empty fields
;
FORWARD_FUNCTION strsplit
;
IF n_elements(separator) EQ 0 THEN separator = ' '
;
IF !version.release EQ '5.1' THEN BEGIN 
  list = str_sep(string, separator, _EXTRA = extra)
  idx = where(list NE '', count)
  IF count GT 0 THEN list = list[idx] ELSE list = ''
ENDIF ELSE BEGIN
  list = strsplit(string, separator, /extract)
  count = n_elements(list)
ENDELSE
;
return, list
END
