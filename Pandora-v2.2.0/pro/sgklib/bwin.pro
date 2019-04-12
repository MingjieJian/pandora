PRO bwin, wno, wide = wide, tall = tall, fScr = fScr, $
          landscape = landscape, portrait = portrait, $
          pc8 = pc8, udc = udc, plct = plct, csz = csz, _EXTRA = extra
;
;+
; bwin, [wno]: 
;
; wrapper around window to open a 1100x920 (or 900x850) 'big' window.
;  depending on display screen size.
;
; additional input keywords:
;
;  fScr      - fraction of screen to cover (def: .85)
;  wide      - set width only  for a 'wide' window
;  tall      - set height only for a 'tall' window
;  landscape - set size to 1000 x  750 (11"x8.5" - 1"x1" @ 10pxl/inch)
;  portrait  - set size to  750 x 1000 
;  pc8       - call pc8 (set visual to 8bit pseudocolor)
;  udc       - call UDC (use decomposed color model)
;  plct      - call plct (set color map to 16 basic color), imply pc8
;  csz       - set !p.charsize = csz
;  _EXTRA    - passed to window
;
; <- Last updated: Wed Jul 22 14:14:05 2009 -> SGK
;-
;  --------------- -
;= BWIN            - wrapper around window to open a big window
; ---------------------------------------------------------------------------
;
IF n_elements(wno)  EQ 0 THEN wno  = 0
IF n_elements(fScr) EQ 0 THEN fScr = 0.85
plct = keyword_set(plct)
pc8  = keyword_set(pc8)
udc  = keyword_set(udc)
;
IF udc THEN UDC
IF pc8 THEN PC8
;
;;IF plct EQ 1 AND (pc8 EQ 0 AND udc EQ 0) THEN PC8
;
IF !d.name NE 'X' THEN GOTO, done
;
tall      = keyword_set(tall)
wide      = keyword_set(wide)
landscape = keyword_set(landscape)
portrait  = keyword_set(portrait)
;
IF !d.name NE 'X' THEN BEGIN
  ;; ***
  IF portrait THEN ps_opts, /portrait
  IF plct THEN plct
  return
ENDIF
;
IF landscape THEN BEGIN
  window, wno, xsize = 1000, ysize = 750, _EXTRA = extra
  GOTO, done
END

IF portrait THEN BEGIN
  window, wno, ysize = 1000, xsize = 750, _EXTRA = extra
  GOTO, done
END
;; my routine is faster, but uses xdpyinfo
IF !d.name EQ 'X' THEN $
  sz = long(XGetScreenSize()) $
ELSE $
  sz = long(get_screen_size())
;; print, sz
xs = round(fScr*(sz[0] < 1920))
ys = round(fScr*sz[1]) 

xs = xs < (sz[0]-50)
ys = ys < (sz[1]-100)

IF wide THEN BEGIN
  window, wno, xsize = xs, _EXTRA = extra
  GOTO, done
END

IF tall THEN BEGIN
  window, wno, ysize = ys, _EXTRA = extra
  GOTO, done
END

window, wno, xsize = xs, ysize = ys, _EXTRA = extra

done:
IF keyword_set(csz)  THEN !p.charsize = csz
IF keyword_set(plct) THEN PLCT

return

END
