      subroutine LUKE
     $(IN,IS,MUX,CALLER,M)
C
C     Rudolf Loeser, 1970 Feb 09
C---- Allocates scratch storage for LAUAN.
C     !DASH
      save
C     !DASH
      integer IN, IS, M, M2, MUX
      character CALLER*(*)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('LUKE')
C     !BEG
      call WGET (IS ,CALLER)
C
      M2 = M**2
C
      IN( 1) = IS
      IN( 2) = IN( 1)+M2
      MUX    = IN( 2)+M2
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('LUKE')
C
      return
      end
