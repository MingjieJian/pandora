      subroutine NUGRIDA
     $(IN,IS,MUX,CALLER,N)
C
C     Rudolf Loeser, 2000 Jun 20
C---- Allocates scratch storage for GUNDARI.
C     !DASH
      save
C     !DASH
      integer IN, IS, LDLMX, MUX, N
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
      call HI ('NUGRIDA')
C     !BEG
      call WGET (IS ,CALLER)
C
      IN( 1) = IS
      IN( 2) = IN( 1)+N*LDLMX
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+N
      IN( 6) = IN( 5)+N
      IN( 7) = IN( 6)+N
      IN( 8) = IN( 7)+N
      IN( 9) = IN( 8)+N
      IN(10) = IN( 9)+N
C
      IN(11) = IN(10)+N
      IN(12) = IN(11)+N
      IN(13) = IN(12)+N
      MUX    = IN(13)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('NUGRIDA')
C
      return
      end
