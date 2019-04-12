      subroutine NODDY
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2004 May 07
C---- Allocates scratch storage for BOTTOM.
C     (This is version 2 of NODDY.)
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, N2
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
      call HI ('NODDY')
C     !BEG
      call WGET (IS,  CALLER)
C
      N2 = N*N
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+N2
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+N
      MUX    = IN( 5)+N
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('NODDY')
C
      return
      end
