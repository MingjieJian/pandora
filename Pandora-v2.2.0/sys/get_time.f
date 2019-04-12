      subroutine GET_TIME
     $(TYME)
C     Rudolf Loeser, 1998 Apr 02
C---- Returns the current clock-on-the-wall time.
C     Format: "HH:MM:SS"
C     !DASH
      save
C     !DASH
      integer I, TIME
      character BLANK*1, TYME*8, ZERO*1
C     !DASH
      external  itime
C
      dimension TIME(3)
C
      data      BLANK, ZERO /' ', '0'/
C
C     !BEG
      call itime (TIME)
      write (TYME,100) TIME
  100 format(I2,':',I2,':',I2)
      do 101 I=1,8
        if(TYME(I:I).eq.BLANK) then
          TYME(I:I) = ZERO
        end if
  101 continue
C     !END
C
      return
      end
