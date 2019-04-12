      subroutine LAVA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1977 Sep 14
C---- Allocates scratch storage for EUDOXIA.
C     (This is version 2 of LAVA.)
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
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('LAVA')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      MUX    = IN( 1)+KM
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('LAVA')
C
      return
      end
