      subroutine KAMBER
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1983 Feb 18
C---- Allocates scratch storage for BANGOR.
C     !DASH
      save
C     !DASH
      integer IN, IS, LDLMX, MUX, N, NLD, NN
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (LEST(33),LDLMX)
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C     !EJECT
C
      call HI ('KAMBER')
C     !BEG
      call WGET (IS,  CALLER)
C
      NN  = N**2
      NLD = N*LDLMX
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+N
      IN( 4) = IN( 3)+NLD
      IN( 5) = IN( 4)+NLD
      IN( 6) = IN( 5)+NLD
      IN( 7) = IN( 6)+N
      IN( 8) = IN( 7)+NN
      IN( 9) = IN( 8)+N
      IN(10) = IN( 9)+N
      IN(11) = IN(10)+N
C
      IN(12) = IN(11)+NLD
      IN(13) = IN(12)+NN
      MUX    = IN(13)+N
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('KAMBER')
C
      return
      end
