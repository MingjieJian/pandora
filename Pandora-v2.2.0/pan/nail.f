      subroutine NAIL
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1974 Mar 21
C---- Allocates scratch storage for HAMMER.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUL, MUX, NL, NM
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
      call HI ('NAIL')
C     !BEG
      call WGET (IS , CALLER)
C
      MUL = NL*(NL-1)/2
      NM  = NL**2
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+MUL
      IN( 3) = IN( 2)+NM
      IN( 4) = IN( 3)+NL
      IN( 5) = IN( 4)+NL
      MUX    = IN( 5)+NL
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('NAIL')
C
      return
      end
