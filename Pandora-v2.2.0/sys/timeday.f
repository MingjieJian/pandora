      subroutine TIMEDAY
     $(DATE,TIME, SDAY)
C     Rudolf Loeser, 1986 May 08
C---- Converts ASCII calendar day and clock time into Smithsonian Day.
C
C     DATE is of the form "YYYY-MMM-DD", TIME is of the form "HH:MM:SS".
C     !DASH
      save
C     !DASH
      real*8 DAY, F, HOUR, OMNTH, SDAY, SEC, SIXTY, SPD, XMIN, YEAR
      integer NOMTH
      logical ERROR
      character DATE*11, MNTH*3, TIME*8
C     !DASH
      external  MONTH, SMTHDAY
C
      data      SIXTY,SPD /6.D1, 8.64D4/
C
C     !BEG
      read (DATE,100) YEAR,MNTH,DAY
  100 format(F4.0,1X,A3,1X,F2.0)
C
      call MONTH   (1,MNTH,NOMTH,ERROR)
      if(ERROR) then
        NOMTH = 0
      end if
      OMNTH = NOMTH
C
      call SMTHDAY (SDAY,YEAR,OMNTH,DAY)
C
C
      read (TIME,101) HOUR,XMIN,SEC
  101 format(F2.0,1X,F2.0,1X,F2.0)
      F = ((HOUR*SIXTY+XMIN)*SIXTY+SEC)/SPD
C
C
      SDAY = SDAY+F
C     !END
C
      return
      end
