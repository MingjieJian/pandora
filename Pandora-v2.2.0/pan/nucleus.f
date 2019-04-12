      subroutine NUCLEUS
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1997 Aug 04
C---- Allocates scratch storage for DROPION.
C     (This is version 3 of NUCLEUS.)
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, NSL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(40),NSL)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('NUCLEUS')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NSL
      MUX    = IN( 2)+NSL**2
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('NUCLEUS')
C
      return
      end
