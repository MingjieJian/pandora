;;
;;  bin/extract VELOCITIES leidca2.aaa.001 leidca2.veloc.out.001
;;  bin/extract VELOCITIES leidca2.aaa.002 leidca2.veloc.out.002
;;
fn1 = 'leidca2.veloc.out.001'
fn2 = 'leidca2.veloc.out.002'
t1 = read_out(fn1, nhdr = 10)
t2 = read_out(fn2, nhdr = 10)
d1 = *t1._dta[0]
d2 = *t2._dta[0]
l1 = *t1._lbl[0]
l2 = *t2._lbl[0]
k0 = 1
bwin, /plct
!p.multi = [0, 3, 3]
!p.charsize = 2
!x.range = [-5e6, 0]
FOR k = 0, 8 DO BEGIN
   plot, d1[k0, *], d2[k, *], title = l1[k], xtitle = l1[k0], psym = -1
  oplot, d2[k0, *], d1[k, *], /color, psym = -4
  IF k EQ 0 THEN BEGIN
    rxyouts, .05, .05, fn2
    rxyouts, .05, .10, fn1, /color
  ENDIF
ENDFOR

!p.multi = 0
!p.charsize = 0
!x.range = 0
END
