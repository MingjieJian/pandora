PRO plct, n = n, offset = offset, invertBW = invertBW
;+
; Name:
;
;        PLCT
; Purpose: 
;
;       Load a `plotting' color map:
;         red, green, blue, yellow, magenta, indigo, etc...
;       up to 14 colors.
;
; Calling Sequence:
;
;       PLCT
;
; Keywords:
;       n = <n> - no of colors [3-14], 
;           default 14, min 3, max 14.
;       offset = <n> - offset at which to load the colmap,
;           default 1, min 1.
;       invertBW = flag, of set invert black and white levels
;
; Side Effects:
;       Modify the color map, using TVLCT.
;
; Example:
;
;  IDL> plct
;  IDL> plot, findgen(3), thick=4, yr=[0, 6]
;  IDL> for i = 1, 14 do oplot,findgen(3) + i/4., color = i, thick = 4
;
; Modification History:
;       Jul  7 1995 Written by SGK, CfA.
;       Jun 29 1998 modified to create a full color map, and deal w/ b/w
;       levels, SGK
;       May 13 2000 no action if TEK window system
;       May 26 2002 call UDC if !D.N_COLORS NE !D.TABLE_SIZE
;-
;  --------------- -
;= PLCT            - Load a `plotting' color map (16 predefined colors)
; ---------------------------------------------------------------------------
;
on_error, 2
;
COMMON colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr
;
IF !d.name EQ 'TEK' THEN return
;
r = [255B,   0B,   0B, 255B,   0B, 255B, 128B, 255B, 128B,   0B,   0B, 128B, 255B, 255B]
g = [  0B, 255B,   0B, 255B, 255B,   0B,   0B, 128B, 192B, 192B, 128B,   0B,   0B, 192B]
b = [  0B,   0B, 255B,   0B, 255B, 255B,   0B,   0B,   0B, 128B, 255B, 255B, 128B, 192B]
;
IF NOT keyword_set(offset) THEN offset = 1
IF NOT keyword_set(n) THEN n = 14
invertBW = keyword_set(invertBW)
IF !d.name EQ 'PS' THEN BEGIN
  x = [r[3], g[3], b[3]]
  r[3:12] = r[4:*]
  g[3:12] = g[4:*]
  b[3:12] = b[4:*]
  r[13] = x[0]
  g[13] = x[1]
  b[13] = x[2]
ENDIF
;
IF n GT 14 THEN n = 14
IF n LT  3 THEN n = 3
IF offset LT 1 THEN offset = 1
;
n = n-1
nc = !D.TABLE_SIZE              ; !D.N_COLORS < 256
;
IF n_elements(r_orig) NE nc THEN r_orig = bytarr(nc)
IF n_elements(g_orig) NE nc THEN g_orig = bytarr(nc)
IF n_elements(b_orig) NE nc THEN b_orig = bytarr(nc)
;
IF invertBW THEN iWhite = 0 ELSE iWhite = nc-1
r_orig[iWhite] = 255B
g_orig[iWhite] = 255B
b_orig[iWhite] = 255B
;
IF invertBW THEN iBlack = nc-1 ELSE iBlack = 0
r_orig[iBlack] = 0B
g_orig[iBlack] = 0B
b_orig[iBlack] = 0B
;
nn = offset+n
r_orig[offset:nn] = r
g_orig[offset:nn] = g
b_orig[offset:nn] = b
;
r_curr = r_orig
g_curr = g_orig
b_curr = b_orig
;
IF !D.NAME EQ 'X' THEN BEGIN
  IF !D.N_COLORS NE !D.TABLE_SIZE THEN UDC
ENDIF
tvlct, r_orig,  g_orig, b_orig
;
return
END
