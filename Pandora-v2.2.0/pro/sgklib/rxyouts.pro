PRO rxyouts, rxpos, rypos, string, $
             _EXTRA = extra
;
;+
; rxyouts: "relative" version of: 
;  
;    xyouts, xPos, yPos, string, ...
;
;    xPos and yPos range being [0., 1.]             
;
;-
;  --------------- -
;= RXYOUTS         - "Relative" version of xyouts
; ---------------------------------------------------------------------------
;
relscale, rxpos, rypos, xpos, ypos
xyouts, xpos, ypos, string, /data, _EXTRA = extra
;
END
