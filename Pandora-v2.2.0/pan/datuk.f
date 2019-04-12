      subroutine DATUK
     $(LEVELS,IS,IE,M)
C
C     Rudolf Loeser, 1990 Dec 04
C---- Encodes part of a plot label.
C     !DASH
      save
C     !DASH
      integer IE, IS, M
      character BLANK*1, LEVELS*20
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external HI, BYE
C
      call HI ('DATUK')
C     !BEG
      if(M.gt.26) then
        write (LEVELS,100) IS,IE
  100   format('for levels',I4,' -',I4)
      else
        LEVELS = BLANK
      end if
C     !END
      call BYE ('DATUK')
C
      return
      end
