      subroutine WOOD
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2005 Nov 04
C---- Allocates scratch storage for DRUID.
C     !DASH
      save
C     !DASH
      integer IN, IS, KM, MUX, NFL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(49),KM )
      equivalence (JZQ(16),NFL)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('WOOD')
C     !BEG
      call WGET (IS,  CALLER)
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+KM
      IN( 3) = IN( 2)+NFL
      IN( 4) = IN( 3)+NFL
      MUX    = IN( 4)+NFL
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('WOOD')
C
      return
      end
