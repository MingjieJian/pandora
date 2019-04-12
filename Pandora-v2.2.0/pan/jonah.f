      subroutine JONAH
     $(IN,IS,MUX,CALLER,KK)
C
C     Rudolf Loeser, 1970 Feb 11
C---- Allocates scratch storage for KOA.
C     !DASH
      save
C     !DASH
      integer IN, IS, KK, MUX
      character CALLER*(*)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('JONAH')
C     !BEG
      call WGET (IS,  CALLER)
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+KK*KK
      IN( 3) = IN( 2)+KK
      MUX    = IN( 3)+KK
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('JONAH')
C
      return
      end
