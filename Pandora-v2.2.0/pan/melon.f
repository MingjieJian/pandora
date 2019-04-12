      subroutine MELON
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 2004 Jan 28
C---- Allocates scratch storage for KURBIS.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N
      character CALLER*(*)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('MELON')
C     !BEG
      call WGET (IS,  CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+N
      IN( 6) = IN( 5)+N
      IN( 7) = IN( 6)+N
      MUX    = IN( 7)+N
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('MELON')
C
      return
      end
