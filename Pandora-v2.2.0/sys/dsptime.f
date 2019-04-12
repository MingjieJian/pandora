      subroutine DSPTIME
     $(DAYS,TIME)
C     Rudolf Loeser, 1986 May 08
C---- Produces an ASCII string signifying delta-time.
C     DAYS is a time interval expressed in days;
C     TIME will be an ASCII string of the form "HHH:MM:SS".
C     !DASH
      save
C     !DASH
      real*8 DAYS, DEL, DLIM, SPD
      integer IH, IM, IS, JM, JS
      character BLANK*1, MESS*9, TIME*9, ZERO*1
C     !DASH
      data      DLIM,SPD,DEL /4.1666666666666667D1, 8.64D4, 2.D-2/
      data      MESS,BLANK,ZERO /'too large', ' ', '0'/
C
C     !BEG
      if(DAYS.lt.DLIM) then
        JS = DAYS*SPD+DEL
        JM = JS/60
        IS = JS-JM*60
        IH = JM/60
        IM = JM-IH*60
        write (TIME,100) IH,IM,IS
  100   format(I3,':',I2,':',I2)
        if(TIME(5:5).eq.BLANK) TIME(5:5) = ZERO
        if(TIME(8:8).eq.BLANK) TIME(8:8) = ZERO
      else
        TIME = MESS
      end if
C     !END
C
      return
      end
