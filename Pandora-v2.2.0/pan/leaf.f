      subroutine LEAF
     $(IN,IS,MUX,CALLER,NMAX,KMAX)
C
C     Rudolf Loeser, 1980 Jun 12
C---- Allocates scratch storage for CARAMEL.
C     !DASH
      save
C     !DASH
      integer IN, IS, KMAX, KN, MUX, NKN, NMAX, NONC
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
      equivalence (LEST(29),NONC )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('LEAF')
C     !BEG
      call WGET (IS ,CALLER)
C
      KN  = KMAX*NONC
      NKN = NMAX*KN
C
      IN( 1) = IS
      IN( 2) = IN( 1)+KN
      IN( 3) = IN( 2)+NMAX
      MUX    = IN( 3)+NKN
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('LEAF')
C
      return
      end
