      subroutine BETTY
     $(LU,K4,K5,K6,MOVING,TIN,TIME)
C
C     Rudolf Loeser, 1980 Jun 05
C---- Prints timing data for SHARON.
C     (This is version 3 of BETTY.)
C     !DASH
      save
C     !DASH
      real*8 TIME, TIN, TOUT
      integer K4, K5, K6, KS, LU
      logical MOVING
      character BLANK*1, LINE*24
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  SECOND, LINER, HI, BYE
      intrinsic max
C
      call HI ('BETTY')
C     !BEG
      call SECOND   (TOUT)
      TIME = TOUT-TIN
C
      if(LU.gt.0) then
        KS = max(K4,K5,K6)
        if(KS.eq.0) then
          call LINER (2,LU)
        end if
C
        LINE = BLANK
        if(MOVING) then
          LINE = 'Angle-dependent Solution'
        end if
C
        write (LU,100) TIME,LINE
  100   format(' ','Calculation time:',F9.3,' sec.',5X,A24)
      end if
C     !END
      call BYE ('BETTY')
C
      return
      end
