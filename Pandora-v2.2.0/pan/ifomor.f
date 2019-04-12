      subroutine IFOMOR
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2000 May 17
C---- Allocates scratch storage for FIMOOR.
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
      call HI ('IFOMOR')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+KM
      IN( 3) = IN( 2)+KM
      MUX    = IN( 3)+KM
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('IFOMOR')
C
      return
      end
