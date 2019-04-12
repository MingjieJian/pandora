FUNCTION XGetScreenSize, noScreens = n
;
;+
;   sz = XGetScreenSize(noScreens=n)
;
;  return the screen(s) size in 2-elements array
;  faster then get_screen_size, but X-Windows only (uses xdpyinfo)
;
;-
;  --------------- -
;= XGetScreenSize  - return the screen(s) size (using xdpyinfo)
; ---------------------------------------------------------------------------
cmd = 'xdpyinfo | grep dimensions | sed '+"'s/x/ /' | awk '{ print $2, $3}'"
spawn, cmd, str
n = n_elements(str)
sz = lonarr(2, n)
FOR i = 0, n-1 DO $
  sz[*, i] = long(strSep(str[i]))
IF n EQ 1 THEN sz = reform(sz)
return, sz
END
