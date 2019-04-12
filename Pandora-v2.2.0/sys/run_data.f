      subroutine RUN_DATA
     $(LU,MODE,N,NL,NT,NONC,KPTM,QELSM,IONST,DATE,TIME,SDAY)
C
C     Rudolf Loeser, 2002 Apr 09
C---- Handles the "run data" in a restart file header:
C     MODE = 1 means: write it;   MODE = 2 means: read it.
C
C     Note: when MODE = 2, then data are returned in DATE, TIME,
C     and SDAY (= Smithsonian Day corresponding to DATE and TIME);
C     when MODE = 1, then DATE, TIME, and SDAY are not used.
C     !DASH
      save
C     !DASH
      real*8 SDAX, SDAY
      integer IONST, KPTM, LU, MODE, N, NL, NONC, NT
      character DATE*11, DATX*11, LABEL*12, LABEX*12, LINE*80, QELSM*8,
     $          TIME*8, TIMX*8
C     !DASH
      external GET_TIME, GET_DATE, TIMEDAY, ABORT
C
      data LABEL /'> Run Data: '/
C
C     !BEG
  100 format(A12,3I5,2I3,2X,A3,I3,3X,A11,1X,A8,3X,F12.5)
C
      if(MODE.eq.1) then
        call GET_DATE (DATX)
        call GET_TIME (TIMX)
        call TIMEDAY  (DATX,TIMX,SDAX)
        write (LU,100) LABEL,N,NL,NT,NONC,KPTM,QELSM(:3),IONST,
     $                 DATX,TIMX,SDAX
C
      else if(MODE.eq.2) then
        rewind LU
  101   continue
          read (LU,102) LINE
  102     format(A80)
          if(LINE(:12).ne.LABEL) then
            goto 101
          end if
          read (LINE,100) LABEX,N,NL,NT,NONC,KPTM,QELSM,IONST,
     $                    DATE,TIME,SDAY
C
      else
        write (*,103) MODE
  103   format(' ','RUN_DATA:  MODE =',I12,', which is not 1 or 2.')
        call ABORT
      end if
C     !END
C
      return
      end
