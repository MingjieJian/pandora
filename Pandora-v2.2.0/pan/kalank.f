      subroutine KALANK
     $(N,NL,LUP,RKI,IQRK,RLI,IQRL,ESG)
C
C     Rudolf Loeser, 1990 Nov 27
C---- Prints rates corrected for upper level charge exchange,
C     for BULAK.
C     !DASH
      save
C     !DASH
      real*8 ESG, RKI, RLI
      integer IQRK, IQRL, LUP, N, NL
      logical PRNTZ
C     !DASH
      external ABJECT, LINER, OMAR, LEI, HI, BYE
C
C               RKI(N,NSL), RLI(N,NSL), ESG(N,NL), IQRK(NSL), IQRL(NSL)
      dimension RKI(*),     RLI(*),     ESG(*),    IQRK(*),   IQRL(*)
C
      data PRNTZ /.false./
C
      call HI ('KALANK')
C     !BEG
      if(LUP.gt.0) then
C
        call ABJECT (LUP)
        write (LUP,100)
  100   format(' ','Results of Upper Level Charge Exchange.  ',
     $             '(Printing controlled by option CHXPRNT)')
C
        call LINER  (2,LUP)
        write (LUP,101)
  101   format(' ','The term ESG = NE*SA*GM')
        call OMAR   (LUP,N,NL,ESG,'Level ',PRNTZ)
C
        call LINER  (2,LUP)
        write (LUP,102)
  102   format(' ','RK - Photoionization Rates including ',
     $             'upper level charge exchange with Hydrogen')
        call OMAR   (LUP,N,NL,RKI,'Level ',PRNTZ)
        call LEI    (IQRK,NL,'RK',LUP)
C
        call LINER  (2,LUP)
        write (LUP,103)
  103   format(' ','RL - Photorecombination Rates including ',
     $             'upper level charge exchange with Hydrogen')
        call OMAR   (LUP,N,NL,RLI,'Level ',PRNTZ)
        call LEI    (IQRL,NL,'RL',LUP)
C
      end if
C     !END
      call BYE ('KALANK')
C
      return
      end
