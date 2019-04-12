      subroutine LEDA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1981 Sep 09
C---- Allocates scratch storage for LEAVES.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, NRPMX
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
      equivalence (LEST( 8),NRPMX)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('LEDA')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NRPMX
      IN( 3) = IN( 2)+NRPMX
      IN( 4) = IN( 3)+NRPMX
      IN( 5) = IN( 4)+NRPMX
      MUX    = IN( 5)+NRPMX
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('LEDA')
C
      return
      end
