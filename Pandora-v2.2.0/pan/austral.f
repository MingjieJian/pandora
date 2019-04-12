      subroutine AUSTRAL
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2003 Nov 24
C---- Allocates scratch storage for ARSENAL.
C     (This is version 2 of AUSTRAL.)
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, NL, NL2
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('AUSTRAL')
C     !BEG
      call WGET (IS , CALLER)
C
      NL2  = NL**2
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NL2
      IN( 3) = IN( 2)+NL2
      IN( 4) = IN( 3)+NL2
      IN( 5) = IN( 4)+NL2
      IN( 6) = IN( 5)+NL2
      MUX    = IN( 6)+NL
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('AUSTRAL')
C
      return
      end
