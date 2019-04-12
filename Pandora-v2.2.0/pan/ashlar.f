      subroutine ASHLAR
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1998 Jun 29
C---- Allocates scratch storage for PYRAMID.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('ASHLAR')
C     !BEG
      call WGET (IS, CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+N
      IN( 6) = IN( 5)+N
      IN( 7) = IN( 6)+N
      IN( 8) = IN( 7)+N
      IN( 9) = IN( 8)+N
      IN(10) = IN( 9)+N
      IN(11) = IN(10)+N
C
      IN(12) = IN(11)+N
      IN(13) = IN(12)+N
      IN(14) = IN(13)+N
      IN(15) = IN(14)+N
      IN(16) = IN(15)+N
      IN(17) = IN(16)+N
      IN(18) = IN(17)+N
      MUX    = IN(18)+N
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('ASHLAR')
C
      return
      end
