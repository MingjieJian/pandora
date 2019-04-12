      subroutine STABOR
     $(IN,IS,MUX,CALLER,M)
C
C     Rudolf Loeser, 1998 Apr 21
C---- Allocates scratch storage for ROBATS.
C     !DASH
      save
C     !DASH
      integer IN, IS, M, MUX
      character CALLER*(*)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('STABOR')
C     !BEG
      call WGET (IS,  CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+M
      IN( 3) = IN( 2)+M
      IN( 4) = IN( 3)+M
      MUX    = IN( 4)+M
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('STABOR')
C
      return
      end
