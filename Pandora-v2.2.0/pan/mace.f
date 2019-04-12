      subroutine MACE
C
C     Rudolf Loeser, 2003 Aug 13
C---- Reads the next input field, which must be ( .
C     (This is version 5 of MACE.)
C     !DASH
      save
C     !DASH
      character LPAREN*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(49),LPAREN)
C     !DASH
      external BLUSH, HI, BYE
C
      call HI ('MACE')
C     !BEG
      call BLUSH (LPAREN)
C     !END
      call BYE ('MACE')
C
      return
      end
