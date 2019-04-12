;;
;; xtwiddle: rewrite of HU's XFAL to modify interactively a T(h) profile 
;;           for use with PANDORA
;;
;;   xtwiddle, 'file.zee', save='save.zee', $
;;       xsize=, ysize=, /xlog, /ylog, /menubar
;;       rubber=, /ajdTGrid
;;
;; NOTES;
;;
;;   the 'rubber' function is f(i)=exp(-(di/r)^2), di=index offset, r=rubber prameter
;;
;;   ReadModelFile() reads a model using a rigid format
;;   SaveModelFile() saved only one model in a new file, using the input rigid format
;;     there is not provision to save several profiles, or to plot more that one reference profile
;;
;; <- Last updated: Thu Oct 24 12:09:43 2013 -> SGK
;;
;; ---------------------------------------------------------------------------
;;
FUNCTION GetDate
;;
;; Get date from system time and return in the form: mm/dd/yy
;;
months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", $
          "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
;;
result = systime()
;;
month  =     strmid(result,  4, 3)
iday   = fix(strmid(result,  8, 2))
iyear  = fix(strmid(result, 22, 2))
idx = where(months EQ month)
imonth = idx[0]+1
string = string(FORMAT='(I2.2, "/", I2.2, "/", I2.2)', imonth, iday, iyear)
;;
return, string
;;
END 
;;
;; ---------------------------------------------------------------------------
;;
FUNCTION ReadModelFile, fileName, debug = debug
;;
;; read a model file, taken from Han's Huitenbroek xfal code
;; is assumes some set/fix rigid format, compatible w/ PANDODRA
;;
debug   = keyword_set(debug)
Nmax    = 200  
Nmodels = 0
;;
model = {model, name: '', Ndep: 0, $
         height: fltarr(Nmax), temp: fltarr(Nmax)}
;;
openr, unit, fileName, /GET_LUN
readf, unit, nModels
IF debug THEN print, 'nModels=', nModels
models = replicate( {model}, nModels )
;;
name = ''  
ndep = 0  
dummyLine = ''  
date = ''
for n = 0, nModels-1 do begin
  readf, unit, FORMAT='(A4, A8)', name, date
  IF debug THEN print, 'model#, name, date=', n+1, ' ', name, ' ', date
  models[n].name = name+' '+date
  readf, unit, ndep
  models[n].ndep = ndep
  
  height = fltarr(ndep)  &  temp = fltarr(ndep)
  readf, unit, dummyLine
  readf, unit, height
  for m = 0,1 do readf, unit, dummyLine
  readf, unit, temp
  readf, unit, dummyLine
  
  models[n].height[0:ndep-1] = height
  models[n].temp[0:ndep-1]   = temp
endfor
close, unit
free_lun, unit
;;
return, models
END
;;
;; ---------------------------------------------------------------------------
;;
PRO SaveModelFile, model, params
;;
;; save the resulting model file, only one profile, 
;; and will overwrite the output fiel if present, it saves the T(h) new profile
;; in the same set/fix rigid format compatible with ReadModelFile() and PANDORA
;; 
fileName = params.saveFn
;;
openw, unit, fileName, /GET_LUN
;;
name = strmid(model.name, 0, 4)+getdate()
nModels = 1
printf, unit, nModels, FORMAT='(I3)'
;; for n=0, Nmodels-1 do begin
;; model = models[n]
  ndep = model.ndep
  printf, unit, name
  printf, unit, ndep, FORMAT='(I5)'
  printf, unit, "Z                 ( >"
  printf, unit, model.height[0:ndep-1], FORMAT='(E15.8, 4E16.8)'
  printf, unit, ") >"
  printf, unit, "TE                ( >"
  printf, unit, model.temp[0:ndep-1],   FORMAT='(E15.8, 4E16.8)'
  printf, unit, ") >"
;; endfor
;;
free_lun, unit
;;
END
;;
;; ---------------------------------------------------------------------------
;;
PRO hline, yval, linestyle = linestyle, ls0 = ls0, _EXTRA = extra
;
;+
;  hline, yval, linestyle = linestyle, ls0 = ls0
;
; overplot an horizontal line at yval (yval can be an array)
;
; Support all OPLOT keywords
;-
;  --------------- -
;= HLINE           - overplot an horizontal line
; ---------------------------------------------------------------------------
;
IF n_elements(yval) EQ 0 THEN yval = 0.
IF NOT keyword_set(linestyle) THEN linestyle = 1
IF keyword_set(ls0) THEN  linestyle = 0
ppsym = !p.psym
!p.psym = 0
;
xx = !x.crange
IF !x.type THEN xx = 10.^xx
yvals = [yval]
FOR i = 0, n_elements(yvals)-1 DO $
  oplot, xx, [1.,1.]*yvals[i], linestyle = linestyle, _EXTRA = extra
;
!p.psym = ppsym
;
END
PRO vline, xval, linestyle = linestyle, ls0 = ls0, _EXTRA = extra
;
;+
;  vline, xval, linestyle = linestyle, ls0 = ls0
;
; overplot a vertical line at xval (xval can be an array)
;
; Support all oplot keywords
;-
;  --------------- -
;= VLINE           - overplot a vertical line
; ---------------------------------------------------------------------------
;
IF n_elements(xval) EQ 0 THEN xval = 0.
IF NOT keyword_set(linestyle) THEN linestyle = 1
IF keyword_set(ls0) THEN  linestyle = 0
ppsym = !p.psym
!p.psym = 0
;
yy =  !y.crange
IF !y.type THEN yy = 10.^yy
xvals = [xval]
FOR i = 0, n_elements(xvals)-1 DO $
  oplot, [1.,1.]*xval[i], yy,  linestyle = linestyle, _EXTRA = extra
;
!p.psym = ppsym
;
END
;;
;; ---------------------------------------------------------------------------
;;
PRO plotModel, model, params, refModel = refModel
;;
;; plot the given model and the ref model
;;    T(h) or log(T(h) vs h or log(hRef-h)
;;
thick   = 2
symsize = 1.5
;;
n = model.ndep-1
p = params.plot
x =  model.height[0:n]
xtitle = 'Height'
IF p.xlog THEN BEGIN
  x = params.href-x
  xtitle = 'Ho - Height'
endif
;;
plot, x, model.temp[0:n], psym = -4, $
  thick = thick, symsize = symsize, $
  xrange = p.xrange, /xstyle, xlog = p.xlog, $
  yrange = p.yrange, /ystyle, ylog = p.ylog, $
  xtitle = xtitle, ytitle = 'Temperature', $
  title = 'model: '+model.name
;;
IF p.xlog THEN vline, params.href, line = 4
;;
IF keyword_set(refModel) THEN BEGIN 
  n = refModel.ndep-1
  x = refModel.height[0:n]
  IF p.xlog THEN x = params.href-x
  oplot, x, refModel.temp[0:n], line = 4, psym = -1
ENDIF  
;;
END
;;
;; ---------------------------------------------------------------------------
;;
PRO setPlotScale, x = x, y = y
;;
;;  set the min/max range of the plot into params, cached in common c_twiddle
;;    using model cached in c_newmodel
;;
COMMON c_twiddle, params
COMMON c_newmodel, model
;;
fx = 0.05
ff = 2.0
;;
n = model.ndep-1
;;
IF keyword_set(x) THEN BEGIN 
  IF params.plot.xlog THEN BEGIN
    params.plot.xrange[0] = min(params.href-model.height[0:n])/ff
    params.plot.xrange[1] = max(params.href-model.height[0:n])*ff
  ENDIF ELSE BEGIN
    dx = (max(model.height[0:n])-min(model.height[0:n]))*fx
    params.plot.xrange[0] = min(model.height[0:n])-dx
    params.plot.xrange[1] = max(model.height[0:n])+dx
  ENDELSE
ENDIF
IF keyword_set(y) THEN BEGIN 
  IF params.plot.ylog THEN BEGIN
    params.plot.yrange[0] = min(model.temp[0:n])/ff
    params.plot.yrange[1] = max(model.temp[0:n])*ff
  ENDIF ELSE BEGIN
    dy = (max(model.temp[0:n])-min(model.temp[0:n]))*fx
    params.plot.yrange[0] = min(model.temp[0:n])-dy
    params.plot.yrange[1] = max(model.temp[0:n])+dy
  ENDELSE  
;;
ENDIF
;;
END
;;
;; ---------------------------------------------------------------------------
;; ---------------------------------------------------------------------------
;;
PRO xtwiddle_setHref
;;
;; set the Href value in the resp widget
;;
COMMON c_twiddle,  params
COMMON c_widgets,  widgets
;;
widget_control, widgets.wHref, set_value = params.hRef
;;
END
;;
;; ---------------------------------------------------------------------------
;;
PRO xtwiddle_setScale, x = x, y = y
;;
;; set the scale value in the corresp widgets
;;
COMMON c_twiddle,  params
COMMON c_widgets,  widgets
;;
IF keyword_set(x) THEN BEGIN 
  widget_control, widgets.wXmin, set_value = params.plot.xrange[0]
  widget_control, widgets.wXmax, set_value = params.plot.xrange[1]
ENDIF
IF keyword_set(y) THEN BEGIN 
  widget_control, widgets.wYmin, set_value = params.plot.yrange[0]
  widget_control, widgets.wYmax, set_value = params.plot.yrange[1]
ENDIF
END
;;
;; ---------------------------------------------------------------------------
;;        
PRO xtwiddle_event, event
;;
;; event handler for xtwiddle
;;
COMMON c_twiddle,  params
COMMON c_models,   models
COMMON c_widgets,  widgets
COMMON c_newmodel, newModel
;;
iCenter = 0                     ; 1 ;???
;;
type = TAG_NAMES(event, /STRUCTURE)
;;
CASE type OF
  ;;
  'WIDGET_BUTTON': BEGIN
    ;;
    WIDGET_CONTROL, event.id, GET_UVALUE = uval
    ;;
    IF strmid(uval, 0, 6) EQ 'Model_' THEN BEGIN 
      ;;
      params.mIndex = fix(strmid(uval, 6, 10))
      newModel = models[params.mIndex]
      setPlotScale, /x, /y
      xtwiddle_setScale,   /x, /y
      n = newModel.nDep
      params.href = 2*newModel.height[n-1]-newModel.height[n-2]
      xtwiddle_setHref
      plotModel, newModel, params
      return
    ENDIF 
    ;;
    CASE uval OF 
      'ignore': return
      'Quit': BEGIN
        IF params.isEdited THEN BEGIN
          msg = ['', $
                 ' +----------------------------------+ ', $
                 ' |                                  | ', $
                 ' | ** Modified profile not saved ** | ', $
                 ' |                                  | ', $
                 ' +----------------------------------+ ', $
                 '', $
                 '  answer YES    to save it,', $
                 '         NO     to exit and not save it,', $
                 '  or     CANCEL to continue.',  '']
          widget_control, /hourglass
          iAnswer = dialog_message(msg, title = 'XTwiddle: please confirm', /question, /cancel,  $
                                   dialog_parent = event.id, center = icenter)
          CASE iAnswer OF
            'Yes': BEGIN
              saveModelFile, newModel, params
               message, /info, 'New profile saved to file "'+params.saveFn+'", exiting now.'
            END
            'No': message, /info, 'New profile NOT saved, exiting anyway.'
            'Cancel': return
          ENDCASE
        ENDIF
        WIDGET_CONTROL, event.top, /DESTROY
        return
      END
      'Reset': BEGIN
         msg = ['', $
                ' +------------------------------------+ ', $
                ' |                                    | ', $
                ' | Abandon changes and reset profile? |',  $
                ' |                                    | ', $
                ' +------------------------------------+ ', $
                '']
         widget_control, /hourglass
         iAnswer = dialog_message(msg, title = 'XTwiddle: please confirm', /question, $
                                  dialog_parent = event.id, center = icenter)
         IF iAnswer EQ 'Yes' THEN BEGIN 
           newModel = models[params.mIndex]
           params.isEdited = 0
           widget_control, widgets.wModels, sensitive = 1
           widget_control, widgets.wSave,   sensitive = 0
           widget_control, widgets.wReset,  sensitive = 0
           plotModel, newModel, params
         ENDIF
       END
      'Save': BEGIN
        saveModelFile, newModel, params
        params.isEdited = 0
        widget_control, widgets.wModels, sensitive = 1
        widget_control, widgets.wSave,   sensitive = 0
        widget_control, widgets.wReset,  sensitive = 0
        msg = ['Info:', '  New profile saved to file "'+params.saveFn+'"', '']
        widget_control, /hourglass
        iAnswer = dialog_message(msg, title = 'XTwiddle: Info', /information, $
                                 dialog_parent = event.id, center = icenter)
        return
      END
      'ResetXScale': BEGIN
        setPlotScale, /x
        xtwiddle_setScale, /x
        plotModel, newModel, params, refModel = models[params.mIndex]
       END
      'ResetYScale': BEGIN
        setPlotScale, /y
        xtwiddle_setScale, /y
        plotModel, newModel, params, refModel = models[params.mIndex]
      END
      'fixHGrid': params.fixHGrid = 1-params.fixHGrid
      ELSE: message, /info, '"'+type+'" event ignored '+uval
      ;;
    ENDCASE 
    ;;
  END
  ;;
  'WIDGET_DROPLIST': BEGIN
    ;;
    WIDGET_CONTROL, event.id, GET_UVALUE = uval
    CASE uval OF
      'Hlin/log': BEGIN
        params.plot.xlog = widget_info(event.id, /droplist_select)
        setPlotScale, /x
        xtwiddle_setScale,   /x
        plotModel, newModel, params, refModel = models[params.mIndex]
      END
      'Tlin/log': BEGIN
        params.plot.ylog = widget_info(event.id, /droplist_select)
        setPlotScale, /y
        xtwiddle_setScale,   /y
        plotModel, newModel, params, refModel = models[params.mIndex]
      END
      'Models': BEGIN
        params.mIndex = widget_info(event.id, /droplist_select)
        newModel = models[params.mIndex]
        setPlotScale, /x, /y
        xtwiddle_setScale,   /x, /y
        n = newModel.nDep
        params.href = 2*newModel.height[n-1]-newModel.height[n-2]
        xtwiddle_setHref
        plotModel, newModel, params
     END
     ELSE: message, /info, '"'+type+'" event ignored '+uval
      ;;
    ENDCASE 
    ;;
  END
  ;;
  '':BEGIN
    ;;
    WIDGET_CONTROL, event.id, GET_UVALUE = uval
    CASE uval OF
      'Hmin': BEGIN
        widget_control, widgets.wXmin, get_value = value
        params.plot.xrange[0] = value
        xtwiddle_setScale,   /x
        plotModel, newModel, params
      END
      'Hmax': BEGIN
        widget_control, widgets.wXmax, get_value = value
        params.plot.xrange[1] = value
        xtwiddle_setScale,   /x
        plotModel, newModel, params
      END
      'Tmin': BEGIN
        widget_control, widgets.wYmin, get_value = value
        params.plot.yrange[0] = value
        xtwiddle_setScale,   /y
        plotModel, newModel, params
      END
      'Tmax': BEGIN
        widget_control, widgets.wYmax, get_value = value
        params.plot.yrange[1] = value
        xtwiddle_setScale,   /y
        plotModel, newModel, params
      END
      ;; 'setN':
      ELSE: message, /info, '"'+type+'" event ignored '+uval
    ENDCASE
  END
  ;;
  'WIDGET_DRAW': BEGIN
    ;;
    WIDGET_CONTROL, event.id, GET_UVALUE = uval
    CASE uval OF 
      'Draw': handle_draw, event
      ELSE: message, /info, '"'+type+'" event ignored' +uval
    ENDCASE
    ;;
  END 
  ;;
  'WIDGET_SLIDER': BEGIN
    WIDGET_CONTROL, event.id, GET_UVALUE = uval
    CASE uval OF
      'rubber': params.rubber = event.value
      ELSE: message, /info, '"'+type+'" event ignored '+uval
    ENDCASE 
  END
  ELSE: message, /info, '"'+type+'" event ignored'
  ;;  
ENDCASE
;;
END
;;
;; ---------------------------------------------------------------------------
;;
FUNCTION fRubber, t, y, iMarked, rubber
;;
;; rubber function
;;
n = n_elements(t)
xr = ((findgen(n)-iMarked)/rubber < 8.) >  (-8.)
ff = exp(-xr^2)
;;
IF iMarked NE 0   THEN ff[0] = 0
IF iMarked NE n-1 THEN ff[n-1] = 0
;;
dt = y-t[iMarked]
tx = t + dt*ff
return, tx
END
;;
;; ---------------------------------------------------------------------------
;;
PRO handle_draw, event
;;
;; drawing window event handler
;;
COMMON c_twiddle,  params
COMMON c_widgets,  widgets
COMMON c_models,   models
COMMON c_newmodel, m
COMMON c_save,     mode, val, savex, savey, iMarked
;;
IF n_elements(mode) EQ 0 THEN mode = 'idle'
;;
co = convert_coord(event.x, event.y, /DEVICE, /TO_DATA )
x = co[0]
y = co[1]
;;
IF event.press THEN BEGIN
  ;;
  savex = x
  savey = y
  DEVICE, SET_GRAPHICS_FUNCTION = 6
  ;;
  ;;'outside'?
  IF x LT params.plot.xrange[0] AND $
    y LT params.plot.yrange[1] AND y GT params.plot.yrange[0] THEN BEGIN
    mode = 'zoom-y'
    val = y
    hline, y
;;    print, mode
    return
  ENDIF
  IF y LT params.plot.yrange[0] AND $
    x LT params.plot.xrange[1] AND x GT params.plot.xrange[0] THEN BEGIN
    mode = 'zoom-x'
    val = x
    vline, x
;;    print, mode
    return
  ENDIF
  ;;
  IF x LT params.plot.xrange[0] OR x GT params.plot.xrange[1] OR $
    y LT params.plot.yrange[0] OR y GT params.plot.yrange[1] THEN BEGIN
    setPlotScale, y = (x LT params.plot.xrange[0]), x = (y LT params.plot.yrange[0])
    xtwiddle_setScale, /x, /y
    return
  ENDIF 
  ;; inside
  mode = 'pressed'
;;  print, mode
  ;;
  n   = m.ndep-1
  IF params.plot.xlog THEN $
    dsq = abs(params.href-m.height[0:n]-x) $
  ELSE $
    dsq = abs(m.height[0:n]-x)^2
  jnk = min(dsq, iMarked)
  ;;
  savey = m.temp[iMarked]
  savex = m.height[iMarked]
  IF params.plot.xlog THEN savex = params.href - savex
  hline, [savey]
  vline, [savex]
  ;;
  IF params.isEdited EQ 0 THEN BEGIN
    params.isEdited = 1
    widget_control, widgets.wModels, sensitive = 0
    widget_control, widgets.wSave,   sensitive = 1
    widget_control, widgets.wReset,  sensitive = 1
  ENDIF
  ;;
  widget_control, widgets.wCH, set_value = m.height[iMarked]
  widget_control, widgets.wCT, set_value = m.temp[iMarked]
  widget_control, widgets.wCN, set_value = iMarked
  ;;  
  return
ENDIF
IF event.release THEN BEGIN
  DEVICE, SET_GRAPHICS_FUNCTION = 3
  CASE mode OF
    'zoom-x': BEGIN
      params.plot.xrange[0] = min([val, x])
      params.plot.xrange[1] = max([val, x])
      xtwiddle_setScale, /x
    END 
    'zoom-y': BEGIN
      params.plot.yrange[0] = min([val, y])
      params.plot.yrange[1] = max([val, y])
      xtwiddle_setScale, /y
    END 
    ;;
    'pressed': BEGIN
      IF params.fixHGrid THEN BEGIN
        ;;
        IF params.rubber EQ 0. THEN BEGIN
          m.temp[iMarked] = savey
        ENDIF ELSE BEGIN 
          t = m.temp[0:m.ndep-1]
          tx = fRubber(t, savey, iMarked, params.rubber)
          m.temp[0:m.ndep-1] = tx
        ENDELSE
        ;;
      ENDIF ELSE BEGIN 
        IF params.rubber EQ 0. THEN BEGIN
          m.height[iMarked] = savex
          m.temp[iMarked]   = savey
        ENDIF ELSE BEGIN 
          ;;
          IF params.plot.xlog THEN BEGIN
            print, 'nothing'
          ENDIF ELSE BEGIN
            ;;
            h = m.height[0:m.ndep-1]
            t = m.temp[0:m.ndep-1]
            ;;
            hx = max(h)
            hn = min(h)
            hh = (fRubber(h, savex, iMarked, params.rubber) > hn) < hx
            tx = fRubber(t, savey, iMarked, params.rubber)        
            hh = hh[sort(hh)]
            tt = interpol(tx, h, hh)
            ;;
            m.height[0:m.ndep-1] = hh
            m.temp[0:m.ndep-1]   = tt
          ENDELSE
        ENDELSE
      ENDELSE
    END
    ELSE: 
  ENDCASE  
  ;;
  plotModel, m, params, refModel = models[params.mIndex]
  mode = 'idle'
  return
ENDIF
;; motion only
CASE mode OF
  'zoom-x': BEGIN
    vline, savex
    vline, [val, x]
    savex = x
  END 
  'zoom-y': BEGIN
    hline, savey
    hline, [val, y]
    savey = y
  END
  'pressed': BEGIN
    ;;
    ;; keep the marked x-location fixed IF x NE savex THEN vline, [x, savex]
    ;;
    IF y NE savey THEN hline, [y, savey]
    ;;
    n = m.ndep-1
    h = m.height[0:n]
    IF params.plot.xlog THEN h = params.href - h
    t = m.temp[0:n]
    ;;
    IF params.fixHGrid THEN BEGIN
      ;;
      IF params.rubber EQ 0. THEN begin
        t[iMarked] = savey
        oplot, h, t, line = 3     
        t[iMarked] = y
        oplot, h, t, line = 3
      ENDIF ELSE BEGIN
        tx = fRubber(t, savey, iMarked, params.rubber)
        oplot, h, tx, line = 3
        tx = fRubber(t, y, iMarked, params.rubber)
        oplot, h, tx, line = 3
      ENDELSE
      ;;
    ENDIF ELSE BEGIN
      ;;
      IF params.rubber EQ 0. THEN begin
        h[iMarked] = savex
        t[iMarked] = savey
        oplot, h, t, line = 3
        ;;
        dh = (h[(iMarked+1) < n] - h[(iMarked-1) > 0])/10.
        IF iMarked EQ 0 THEN hn = 2*h[0] ELSE hn = h[iMarked-1] + dh
        IF iMarked EQ n THEN hx = 2*h[n] ELSE hx = h[iMarked+1] - dh
        x = (x >  hn) < hx
        h[iMarked] = x
        t[iMarked] = y
        oplot, h, t, line = 3
        ;;
      ENDIF ELSE BEGIN
        ;;
        hx = max(h)
        hn = min(h)     
        ;;
        hh = (fRubber(h, savex, iMarked, params.rubber) > hn) < hx
        tx = fRubber(t, savey, iMarked, params.rubber)        
        hh = hh[sort(hh)]
        tt = interpol(tx, h, hh)
        oplot, hh, tt, line = 3, psym = -4
        oplot, [hh[iMarked]], [tt[iMarked]],  psym =  4, symsize = 4;, thick = 3
        ;; 
        xn = h[(iMarked-params.rubber) > 0]
        xx = h[(iMarked+params.rubber) < n]
        x =  (x > xn) < xx
        hh = (fRubber(h, x, iMarked, params.rubber) > hn) < hx
        tx = fRubber(t, y, iMarked, params.rubber)       
        hh = hh[sort(hh)]
        tt = interpol(tx, h, hh)
        oplot, hh, tt, line = 3, psym = -4
        oplot, [hh[iMarked]], [tt[iMarked]],  psym =  4, symsize = 4;, thick = 3
          
      ENDELSE

    ENDELSE
    IF params.fixHGrid EQ 0 THEN $
      widget_control, widgets.wCH, set_value = x
    widget_control, widgets.wCT, set_value = y
    ;;
    savex = x
    savey = y
  END
  ELSE: ;; print, '*'
ENDCASE
;;
END
;;
;; ---------------------------------------------------------------------------
;;
FUNCTION initWidgets, mlist, xsize, ysize, menubar = mbar
;;
;; initialize all the widgets, retun a structure w/ widgets needed for control
;;
COMMON c_twiddle, params
;;
version = 'Ver 1.0/3/Oct2013/SGK'
;;
;; use a menu-bar?
IF keyword_set(mbar) THEN BEGIN 
  ;;
  wTop = widget_base(MBAR = menuBarW, TITLE='XTwiddle - '+version)
  ;;
  menuW  = WIDGET_BUTTON(menuBarW, VALUE = 'Menu', /MENU)
  wSave  = WIDGET_BUTTON(menuW, VALUE = 'Save',  UVALUE = 'Save',sensitive = 0)
  wReset = WIDGET_BUTTON(menuW, VALUE = 'Reset', UVALUE = 'Reset', sensitive = 0)
  wQuit  = WIDGET_BUTTON(menuW, VALUE = 'Quit',  UVALUE = 'Quit')
  ;;
  w = WIDGET_BUTTON(menuW, VALUE = 'File: "'+params.filename+'"', sensitive = 1, uvalue = 'ignore', /separator)
  wModels = WIDGET_BUTTON(menuW, VALUE = 'Select Model', /menu)
  FOR i = 0, n_elements(mlist)-1 DO $
    w = WIDGET_BUTTON(wModels, VALUE = mlist[i], UVALUE = 'Model_'+string(i, format = '(i0)'))
  wBase0 = widget_base(wTop,   /FRAME, /COL )
ENDIF ELSE BEGIN 
  ;;
  wTop   = widget_base(TITLE = 'XTwiddle - '+version, /col)
  wBase0 = wTop
  wBase1  = widget_base(wBase0, /FRAME, /ROW )
  wRes    = WIDGET_LABEL(wBase1, value = 'file: "'+params.filename+'", model in file:')
  wModels = WIDGET_DROPLIST(wBase1, VALUE = mlist, UVALUE = 'Models')
  ;;
  wSave  = WIDGET_BUTTON(wBase1, VALUE = 'Save',  UVALUE = 'Save',  xsize = 150, sensitive = 0)
  wReset = WIDGET_BUTTON(wBase1, VALUE = 'Reset', UVALUE = 'Reset', xsize = 150, sensitive = 0)
  wQuit  = WIDGET_BUTTON(wBase1, VALUE = 'Quit',  UVALUE = 'Quit',  xsize = 150)
ENDELSE
;;
wBase1 = widget_base(wBase0, /FRAME, /ROW )
wDraw  = widget_draw(wBase1, XSIZE = xsize, YSIZE = ysize, $
                     /BUTTON_EVENTS, /MOTION_EVENTS, UVALUE='Draw' )
drawWindow = !d.window
;;
p = params.plot
xfsize = 12
;;
wBase1 = widget_base(wBase0, /FRAME, /ROW )
wLabel = WIDGET_LABEL(wBase1, value = 'Plot range:')
;;
wDrop = WIDGET_DROPLIST(wBase1, VALUE = ['H', 'log(Ho-H)'], UVALUE = 'Hlin/log')
widget_control, wDrop, set_droplist_select = p.xlog
wXmin = cw_field(wBase1, /RETURN_EVENTS, $
             TITLE='H_min:', /FLOAT, UVALUE= 'Hmin', xsize = xfsize, value = p.xrange[0])
wXMax = cw_field(wBase1, /RETURN_EVENTS, $
             TITLE='H_max:', /FLOAT, UVALUE= 'Hmax', xsize = xfsize, value = p.xrange[1])
w = WIDGET_BUTTON(wBase1, VALUE = 'Reset scale(H)', UVALUE = 'ResetXScale')
;;
wDrop = WIDGET_DROPLIST(wBase1, VALUE =  ['T', 'log(T)'], UVALUE = 'Tlin/log')
widget_control, wDrop, set_droplist_select = p.ylog
wYmin = cw_field(wBase1, /RETURN_EVENTS, $
             TITLE='T_min:', /FLOAT, UVALUE= 'Tmin', xsize = xfsize, value = p.yrange[0])
wYmax = cw_field(wBase1, /RETURN_EVENTS, $
             TITLE='T_max:', /FLOAT, UVALUE= 'Tmax', xsize = xfsize, value = p.yrange[1])
w = WIDGET_BUTTON(wBase1, VALUE = 'Reset scale(T)', UVALUE = 'ResetYScale')

;;
wBase1 = widget_base(wBase0, /FRAME, /ROW )
wRes = WIDGET_LABEL(wBase1, value = 'Current Point:')
wCH = cw_field(wBase1, /RETURN_EVENTS, $
             TITLE='H:', /FLOAT, UVALUE= 'curH', xsize = xfsize, value = 0., /noedit)
wCT = cw_field(wBase1, /RETURN_EVENTS, $
             TITLE='T:', /FLOAT, UVALUE= 'curT', xsize = xfsize, value = 0., /noedit)
wCN = cw_field(wBase1, /RETURN_EVENTS, $
             TITLE='n:', /INT, UVALUE= 'setN', xsize = 5, value = -1, /noedit )

wHRef = cw_field(wBase1, /RETURN_EVENTS, $
             TITLE='Ho:', /FLOAT, UVALUE= 'setHref', xsize = 5, value = params.href, /noedit )
;;
wBase2 = widget_base(wBase1, /FRAME, /ROW, /nonexclusive)
w = widget_button(wBase2, value = 'Height grid is fixed', uvalue = 'fixHGrid')
widget_control, w, set_button = params.fixHGrid
;;
w = widget_label(wBase1, value = 'Rubber: ')
w = widget_slider(wBase1, MINIMUM = 0, MAXIMUM = 5, $
                  uvalue = 'rubber', value = params.rubber)
;;
widget_control, /REALIZE, wTop
return, {widgets, wTop:wTop, wModels:wModels, wSave:wSave, wReset:wReset, $
         drawWindow:drawWindow, $
         wXmin:wXmin, wXmax:wXmax, $
         wYmin:wYmin, wYmax:wYmax, $
         wCH:wCH, wCT:wCT, wCN:wCN, wHRef:wHRef}
END
;;
;; ---------------------------------------------------------------------------
;;
PRO xtwiddle, fileName, save = saveFn, $
              xsize = xsize, ysize = ysize, $
              xlog = xlog, ylog = ylog, $
              menubar = menubar, rubber = rubber, adjHGrid = adjHGrid
;;
;; adjust interactively a T(h) profile and save it to a new file
;;
COMMON c_twiddle,  params
COMMON c_models,   models
COMMON c_widgets,  widgets
COMMON c_newmodel, newModel
;;
IF NOT keyword_set(saveFn) THEN saveFn = 'save.zee'
IF NOT keyword_set(xsize)  THEN xsize  = 1000
IF NOT keyword_set(ysize)  THEN ysize  = 800
IF NOT keyword_set(xlog)   THEN xlog   = 0
IF NOT keyword_set(ylog)   THEN ylog   = 0
IF NOT keyword_set(rubber) THEN rubber = 3 ; 0
IF NOT keyword_set(adjTGrid) THEN fixHGrid = 1 ELSE fixHGrid = 0
rubber = (fix(rubber) > 0) < 5

IF n_params(0) LT 1 THEN  BEGIN
  print, "Usage: xtwiddle, 'file.zee'"
  print, 'options: save=,              -- name of save file, def:. '+"'save.zee'"
  print, '         xsize=, ysize=      -- size of plotting window, def: 1200x1000'
  print, '         /xlog, /ylog        -- plot log(Ho-H) or log(T)'
  print, '         /menubar            -- use menu bar instead of buttons'
  print, '         rubber=             -- "rubber" parameter.'
  return
ENDIF
params = {params, fileName:fileName, mIndex:0, hRef:0., $          
          saveFn:saveFn, iSave:0, isEdited:0, fixHGrid:fixHGrid, rubber:rubber, $
          plot:{plot, xlog:xlog, ylog:ylog, $
                xrange:[0., 0.], yrange:[0., 0.]}}
;;
;; read the model file: hold several models
;;
models   = ReadModelFile(fileName)
;; selet the new model
newModel = models[params.mIndex]
;;
n = newModel.nDep
params.href = 2*newModel.height[n-1]-newModel.height[n-2]
;;
;; set the plot limits
setPlotScale, /x, /y
;; initialize the widgets
widgets = initWidgets(models.name, xsize, ysize, menubar = menubar)
;;
;; wSet, window(widegts.wDraw) ???
;;
plotModel, newModel, params
;;
xmanager, 'xtwiddle', widgets.wTop
;;
END
