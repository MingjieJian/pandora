      subroutine DRALENA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2003 Mar 25
C---- Allocates scratch storage for GADUMI.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NNV, NVX
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(42),NVX)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('DRALENA')
C     !BEG
      call WGET (IS , CALLER)
C
      NNV = N*(NVX+3)
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NNV
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+NNV
      MUX    = IN( 5)+NNV
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('DRALENA')
C
      return
      end
