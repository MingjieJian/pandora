      subroutine SAW
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1997 Aug 27
C---- Allocates scratch storage for PAW.
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
      call HI ('SAW')
C     !BEG
      call WGET (IS,  CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+4*N
      IN( 3) = IN( 2)+2*N
      IN( 4) = IN( 3)+N
      MUX    = IN( 4)+N
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('SAW')
C
      return
      end
