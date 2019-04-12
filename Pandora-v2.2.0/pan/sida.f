      subroutine SIDA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1989 Jan 31
C---- Allocates scratch storage for BERGAMO.
C     !DASH
      save
C     !DASH
      integer IN, IS, LDLMX, MUX
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
      equivalence (LEST(33),LDLMX)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('SIDA')
C     !BEG
      call WGET (IS,  CALLER)
C
      IN( 1) = IS
C
      MUX    = IN( 1)+LDLMX
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('SIDA')
C
      return
      end
