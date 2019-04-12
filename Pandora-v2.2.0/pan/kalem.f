      subroutine KALEM
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1997 Sep 16
C---- Allocates integer scratch storage for ADELMA.
C     (This is version 2 of KALEM.)
C     !DASH
      save
C     !DASH
      integer IN, IS, LCOW, LSIZE, MUX
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
      equivalence (LEST(44),LCOW )
C     !DASH
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('KALEM')
C     !BEG
      call IGET (IS ,CALLER)
C
      LSIZE = LCOW+2
C
      IN( 1) = IS
      IN( 2) = IN( 1)+LSIZE
      MUX    = IN( 2)+LSIZE
C
      call ILCK (MUX,CALLER)
C     !END
      call BYE ('KALEM')
C
      return
      end
