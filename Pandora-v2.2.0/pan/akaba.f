      subroutine AKABA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1995 Aug 24
C---- Allocates integer scratch storage for CARAMEL.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, NONC
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
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('AKABA')
C     !BEG
      call IGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NONC
      IN( 3) = IN( 2)+NONC
      IN( 4) = IN( 3)+NONC
      MUX    = IN( 4)+NONC
C
      call ILCK (MUX,CALLER)
C     !END
      call BYE ('AKABA')
C
      return
      end
