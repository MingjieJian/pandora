      subroutine CHI
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1997 Oct 15
C---- Allocates scratch storage for PUR.
C     (This is version 2 of CHI.)
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
      call HI ('CHI')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      MUX    = IN( 4)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('CHI')
C
      return
      end
