      subroutine INEZ
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2000 Jul 24
C---- Allocates scratch storage for AZPRIN.
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
      call HI ('INEZ')
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
      call BYE ('INEZ')
C
      return
      end
