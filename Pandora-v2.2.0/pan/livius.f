      subroutine LIVIUS
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1983 Feb 24
C---- Allocates scratch storage for FROLLO.
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
C     !EJECT
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('LIVIUS')
C     !BEG
      call WGET (IS,  CALLER)
C
      NN  = N**2
      NLD = N*LDLMX
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+N
      IN( 3) = IN( 2)+NLD
      IN( 4) = IN( 3)+NLD
      IN( 5) = IN( 4)+NLD
      IN( 6) = IN( 5)+N
      IN( 7) = IN( 6)+NN
      IN( 8) = IN( 7)+N
      IN( 9) = IN( 8)+N
      IN(10) = IN( 9)+N
      IN(11) = IN(10)+NLD
C
      IN(12) = IN(11)+NN
      MUX    = IN(12)+N
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('LIVIUS')
C
      return
      end
