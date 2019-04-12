      subroutine BANTER
     $(C,KNT,LINE)
C
C     Rudolf Loeser, 1996 Apr 12
C---- Encodes a line of CHECK values, for BANOIC.
C     (KNT .le. 10)
C     !DASH
      save
C     !DASH
      real*8 C, RANGE, W
      integer I, KNT
      character BLANK*1, LINE*102
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external MOVE1, BOUNDS, HI, BYE
C
C               C(KNT)
      dimension C(*)
C
      dimension W(10)
C
      data RANGE /99.999999D+0/
C
      call HI ('BANTER')
C     !BEG
      call MOVE1  (C, KNT, W)
      call BOUNDS (KNT, -RANGE, W, RANGE)
C
      LINE = BLANK
      write (LINE,101) (W(I),I=1,KNT)
  101 format(5F10.6,2X,5F10.6)
C     !END
      call BYE ('BANTER')
C
      return
      end
