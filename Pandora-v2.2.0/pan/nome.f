      subroutine NOME
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1970 Feb 18
C---- Allocates scratch storage for AMERICA.
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
      call HI ('NOME')
C     !BEG
      call WGET (IS,  CALLER)
C
      NL2 = NL**2
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NL2
      IN( 3) = IN( 2)+NL
      IN( 4) = IN( 3)+NL
      MUX    = IN( 4)+NL2
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('NOME')
C
      return
      end
