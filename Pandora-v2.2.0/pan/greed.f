      subroutine GREED
     $(A,B,J)
C
C     Rudolf Loeser, 1972 Dec 29
C---- Determines a relationship symbol, for GIMEL.
C     !DASH
      save
C     !DASH
      real*8 A, B
      character BLANK*1, EQUAL*1, I*1, J*1, LARROW*1, RARROW*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
      equivalence (SYMBS(44),EQUAL )
      equivalence (SYMBS(51),LARROW)
      equivalence (SYMBS(52),RARROW)
C     !DASH
      external HI, BYE
C
      dimension J(2)
C
      call HI ('GREED')
C     !BEG
      J(1) = BLANK
      if(A.lt.B) then
        I = LARROW
      else if(A.eq.B) then
        I = EQUAL
      else
        I = RARROW
      end if
      if(I.ne.J(2)) then
        J(2) = I
        J(1) = I
      end if
C     !END
      call BYE ('GREED')
C
      return
      end
