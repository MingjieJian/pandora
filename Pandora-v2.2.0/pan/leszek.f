      subroutine LESZEK
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1997 Sep 26
C---- Allocates scratch storage for GIGGLE.
C     (This is version 2 of LESZEK.)
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
      call HI ('LESZEK')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      MUX    = IN( 1)+NL*NL
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('LESZEK')
C
      return
      end
