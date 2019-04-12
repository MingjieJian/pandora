      subroutine MOSEL
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1988 Jul 28
C---- Allocates scratch storage for RHEIN.
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
      call HI ('MOSEL')
C     !BEG
      call WGET (IS ,CALLER)
C
      NL2 = NL**2
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NL
      IN( 3) = IN( 2)+NL2
      MUX    = IN( 3)+NL2
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('MOSEL')
C
      return
      end
