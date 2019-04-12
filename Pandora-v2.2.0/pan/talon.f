      subroutine TALON
     $(NO,N,NL,RK,IQRK,RL,IQRL,CK,HJ,JH1,JH2)
C
C     Rudolf Loeser, 1979 Nov 26
C---- Prints, for RATES.
C     !DASH
      save
C     !DASH
      real*8 CK, HJ, RK, RL
      integer IQRK, IQRL, JH1, JH2, N, NL, NO
      logical PRNTZ
      character BLANK*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external LINER, ABJECT, OMAR, LEI, HI, BYE
C
C               RK(N,NSL), IQRK(NSL), RL(N,NSL), IQRL(NSL), CK(N,NSL),
      dimension RK(*),     IQRK(*),   RL(*),     IQRL(*),   CK(*),
C
C               HJ(N)
     $          HJ(*)
C
      data PRNTZ /.false./
C     !EJECT
C
      call HI ('TALON')
C     !BEG
      if(NO.gt.0) then
        call ABJECT  (NO)
C
        if(JH2.gt.0) then
          call LINER (2,NO)
          write (NO,100) JH1,JH2
  100     format(' ','HJ - Photoionization rates multiplier',5X,'(',I3,
     $               ',',I3,')')
          call OMAR  (NO,N,1,HJ,BLANK,PRNTZ)
        end if
C
        call LINER   (2,NO)
        write (NO,101)
  101   format(' ','RK - Photoionization rates')
        call OMAR    (NO,N,NL,RK,'Level ',PRNTZ)
        call LEI     (IQRK,NL,'RK',NO)
C
        call LINER   (2,NO)
        write (NO,102)
  102   format(' ','RL - Photorecombination rates')
        call OMAR    (NO,N,NL,RL,'Level ',PRNTZ)
        call LEI     (IQRL,NL,'RL',NO)
C
        call LINER   (2,NO)
        write (NO,103)
  103   format(' ','CK - Collisional ionization rates')
        call OMAR    (NO,N,NL,CK,'Level ',PRNTZ)
      end if
C     !END
      call BYE ('TALON')
C
      return
      end
