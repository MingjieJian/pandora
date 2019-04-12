      subroutine DRIZZLE
     $(IN,LIN)
C
C     Rudolf Loeser, 1980 Feb 15
C---- Encodes "IN" into character form, for HAIL.
C     (This is version 3 of DRIZZLE.)
C     !DASH
      save
C     !DASH
      integer IN
      character BLANK*1, LIN*(*)
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  ACHILES, HI, BYE
C
      call HI ('DRIZZLE')
C     !BEG
      if(IN.eq.0) then
        LIN = BLANK
      else
        call ACHILES (IN,LIN)
      end if
C     !END
      call BYE ('DRIZZLE')
C
      return
      end
