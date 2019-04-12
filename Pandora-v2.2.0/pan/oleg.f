      subroutine OLEG
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1970 Feb 18
C---- Allocates scratch storage for RAVI.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, NL
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
      call HI ('OLEG')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NL
      IN( 3) = IN( 2)+NL
      MUX    = IN( 3)+NL
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('OLEG')
C
      return
      end
