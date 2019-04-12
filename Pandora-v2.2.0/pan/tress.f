      subroutine TRESS
     $(LINE,TC,W,EI,K,MXK,KSTAR,TITLE)
C
C     Rudolf Loeser, 1974 Nov 25
C---- Composes part of a print line for BRAID.
C     !DASH
      save
C     !DASH
      real*8 B, EI, RAT, TC, W
      integer K, KSTAR, MXK
      character BLANK*1, DOLLAR*1, LINE*30, SIG*1, TITLE*3
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(37),DOLLAR)
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external DIVIDE, PLUNCK, HI, BYE
C
      call HI ('TRESS')
C     !BEG
      SIG = BLANK
      if(K.gt.MXK) then
        SIG   = DOLLAR
        KSTAR = 1
      end if
C
      call PLUNCK (W,TC,B)
      call DIVIDE (B,EI,RAT)
C
      write (LINE,100) TITLE,TC,SIG,RAT
  100 format(A3,' = ',F9.0,A1,1PE11.3)
C     !END
      call BYE ('TRESS')
C
      return
      end
