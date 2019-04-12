      subroutine NEEDLE
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 1971 Jul 07 (revised 2000 Jan 25)
C---- Allocates scratch storage for PINE.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, N2
      character CALLER*(*)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('NEEDLE')
C     !BEG
      call WGET (IS ,CALLER)
C
      N2 = 2*N
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N2
      IN( 3) = IN( 2)+N2
      IN( 4) = IN( 3)+N2
      IN( 5) = IN( 4)+N2
      IN( 6) = IN( 5)+N2
      IN( 7) = IN( 6)+N2
      IN( 8) = IN( 7)+N2
      MUX    = IN( 8)+N2
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('NEEDLE')
C
      return
      end
