      subroutine NEDA
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1991 Oct 11
C---- Allocates scratch storage for CLARUS.
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
      call HI ('NEDA')
C     !BEG
      call WGET (IS,  CALLER)
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+N
      MUX    = IN( 2)+N
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('NEDA')
C
      return
      end
