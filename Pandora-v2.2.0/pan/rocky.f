      subroutine ROCKY
     $(LEVEL,NSL,LINE)
C
C     Rudolf Loeser, 1980 Mar 05
C---- Encodes a heading, for PEACH.
C     !DASH
      save
C     !DASH
      integer LEVEL, NSL
      character BLANK*1, LINE*60
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
C
      call HI ('ROCKY')
C     !BEG
      LINE = BLANK
C
      if(LEVEL.le.(NSL+1)) then
C
        if(LEVEL.le.NSL) then
          write (LINE(8:15),100) LEVEL
  100     format('Level',I3)
        else
          LINE(9:15) = 'K-Shell'
        end if
C
        LINE(21:30) = 'Wavelength'
        LINE(36:39) = 'RRNU'
        LINE(47:50) = 'RRCP'
        LINE(55:60) = 'Method'
      end if
C     !END
      call BYE ('ROCKY')
C
      return
      end
