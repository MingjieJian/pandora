      subroutine LAZULI
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1999 Nov 05
C---- Allocates scratch storage for TOURMAL.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, N, NL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('LAZULI')
C     !BEG
      call WGET (IS,  CALLER)
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+N*NL
      MUX    = IN( 3)+NL
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('LAZULI')
C
      return
      end
