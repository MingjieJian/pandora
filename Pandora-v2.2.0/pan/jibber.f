      subroutine JIBBER
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1987 Nov 18
C---- Allocates scratch storage for DUGOUT.
C     !DASH
      save
C     !DASH
      integer IN, IS, LCOW, LNGTH, MUX, NCB, NCL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(51),NCL)
      equivalence (JZQ(55),NCB)
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
      call HI ('JIBBER')
C     !BEG
      call WGET (IS,  CALLER)
C
      LNGTH = LCOW*(2*NCL-1)+2*NCB
C
      IN( 1) = IS
C
      MUX    = IN( 1)+LNGTH
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('JIBBER')
C
      return
      end
