      subroutine IMI
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1991 Aug 01
C---- Allocates scratch storage for DAUCUS.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NVH
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(54),NVH)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('IMI')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+NVH
      IN( 4) = IN( 3)+N
      MUX    = IN( 4)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('IMI')
C
      return
      end
