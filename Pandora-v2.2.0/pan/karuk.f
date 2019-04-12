      subroutine KARUK
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1981 May 08
C---- Allocates scratch storage for HOLDA.
C     !DASH
      save
C     !DASH
      integer IN, IS, LG, MUX, N
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(34),LG )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('KARUK')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      MUX    = IN( 1)+N*LG
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('KARUK')
C
      return
      end
