PRO relscale, rx, ry, x, y
;
;+
; PRO relscale, rx, ry, x, y
;
; Relative scaling:
;   return data scaling (x,y) defined in terms of relative coords (rx,ry)
;
;-
;  --------------- -
;= RELSCALE        - Relative scaling, convert relative unit to data unit
; ---------------------------------------------------------------------------
dx = !x.crange[1]-!x.crange[0]
x  = !x.crange[0]+ rx*dx
IF !x.type EQ 1 THEN x  = 10.^x
;
dy = !y.crange[1]-!y.crange[0]
y  = !y.crange[0]+ ry*dy
IF !y.type EQ 1 THEN y  = 10.^y
;
END
