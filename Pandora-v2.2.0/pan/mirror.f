      subroutine MIRROR
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1973 Apr 04
C---- Allocates scratch storage for SNIT.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, MWV, NW
      character CALLER*(*)
C     !DASH
      external MAROZIA, WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('MIRROR')
C     !BEG
      call WGET    (IS ,CALLER)
C
      call MAROZIA (MWV)
      NW = MWV+1
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NW
      IN( 3) = IN( 2)+NW
      IN( 4) = IN( 3)+NW
      IN( 5) = IN( 4)+NW
      MUX    = IN( 5)+NW
C
      call WLCK    (MUX,CALLER)
C     !END
      call BYE ('MIRROR')
C
      return
      end
