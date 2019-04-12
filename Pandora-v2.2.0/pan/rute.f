      subroutine RUTE
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2005 Nov 07
C---- Allocates scratch storage for RIBBLE.
C     !DASH
      save
C     !DASH
      integer IN, IS, KLYNF, MUX
      character CALLER*(*)
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(81),KLYNF)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('RUTE')
C     !BEG
      call WGET (IS,  CALLER)
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+KLYNF
      MUX    = IN( 2)+KLYNF
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('RUTE')
C
      return
      end
