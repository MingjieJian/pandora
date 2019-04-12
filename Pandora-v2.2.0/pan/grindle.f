      subroutine GRINDLE
     $(ARR,QAR,KNT)
C
C     Rudolf Loeser, 1984 Dec 21
C---- Converts "IRR" into a "yes/no" message.
C     !DASH
      save
C     !DASH
      real*8 ARR
      integer I, J, KNT
      character BLANK*1, QAR*10, SIG*10
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  HI, BYE
      intrinsic min
C
C               ARR(8), QAR(16)
      dimension ARR(*), QAR(*)
C
      dimension SIG(3)
C
      data SIG /'        No', '       Yes', '     Huh ?'/
C
      call HI ('GRINDLE')
C     !BEG
      do 100 I = 1,KNT
C
        QAR(I) = BLANK
        J = ARR(I)
C
        if(J.gt.0) then
          J = min(J,3)
          QAR(I) = SIG(J)
        end if
C
  100 continue
C     !END
      call BYE ('GRINDLE')
C
      return
      end
