      subroutine MICE
C
C     Rudolf Loeser, 2003 Aug 13
C---- Reads the next input field, which must be ) .
C     (This is version 3 of MICE.)
C     !DASH
      save
C     !DASH
      character RPAREN*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(50),RPAREN)
C     !DASH
      external BLUSH, HI, BYE
C
      call HI ('MICE')
C     !BEG
      call BLUSH (RPAREN)
C     !END
      call BYE ('MICE')
C
      return
      end
