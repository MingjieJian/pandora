      subroutine INAUS
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1987 Nov 17
C---- Allocates scratch storage for HERBERT.
C     !DASH
      save
C     !DASH
      integer IN, IS, LCOW, LNGTH, MUX, NCL, NMORE
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(51),NCL)
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(44),LCOW )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('INAUS')
C     !BEG
      call WGET (IS,  CALLER)
C
      LNGTH = LCOW+2
      NMORE = 2*NCL
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+LNGTH
      MUX    = IN( 2)+NMORE
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('INAUS')
C
      return
      end
