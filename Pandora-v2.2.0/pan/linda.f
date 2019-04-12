      subroutine LINDA
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1997 Aug 07
C---- Allocates scratch storage for SIGMA.
C     (This is version 5 of LINDA.)
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
      call HI ('LINDA')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      MUX    = IN( 1)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('LINDA')
C
      return
      end
