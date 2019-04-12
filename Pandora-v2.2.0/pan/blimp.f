      subroutine BLIMP
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2005 Jan 06
C---- Allocates scratch storage for BLOAT.
C     !DASH
      save
C     !DASH
      integer IN, IS, KM, MUX
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(49),KM )
C     !DASH
      external WGET, WLCK
C
      dimension IN(*)
C
      call HI ('BLIMP')
C     !BEG
      call WGET (IS,  CALLER)
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+KM
      IN( 3) = IN( 2)+KM
      IN( 4) = IN( 3)+KM
      IN( 5) = IN( 4)+KM
      MUX    = IN( 5)+KM
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('BLIMP')
C
      return
      end
