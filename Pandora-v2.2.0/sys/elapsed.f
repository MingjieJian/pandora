      subroutine ELAPSED
     $(BEGDAT,BEGTIM,ENDDAT,ENDTIM,DAYS,STRING)
C     Rudolf Loeser, 1986 May 08
C---- Given a 'beginning date/time" and an "ending date/time",
C     this routine computes their difference in DAYS, and
C     constructs an ASCII string representing this difference
C     in the form "HHH:MM:SS".
C
C---- BEGDAT and ENDDAT must have the format "YYYY-MMM-DD",
C     and
C     BEGTIM and ENDTIM must have the format "HH:MM:SS".
C     !DASH
      save
C     !DASH
      real*8 DAYS, START, STOP, ZERO
      character BEGDAT*11, BEGTIM*8, CRAZY*9, ENDDAT*11, ENDTIM*8,
     $          NULL*9, STRING*9
C     !DASH
      external  TIMEDAY, DSPTIME
C
      data      ZERO /0.D0/
      data      CRAZY,NULL /'  crazy  ', '  0:00:00'/
C
C     !BEG
      call TIMEDAY   (BEGDAT,BEGTIM,START)
      call TIMEDAY   (ENDDAT,ENDTIM,STOP )
C
      DAYS = STOP-START
C
      if(DAYS.gt.ZERO) then
        call DSPTIME (DAYS,STRING)
      else if(DAYS.lt.ZERO) then
        STRING = CRAZY
      else
        STRING = NULL
      end if
C     !END
C
      return
      end
