      subroutine KINOC
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1983 Feb 23
C---- Allocates scratch storage for THANET.
C     !DASH
      save
C     !DASH
      integer IN, IS, MRR, MUX, N, NMRR, NN, NNSHL, NSHL
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(15),MRR)
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
      equivalence (LEST( 4),NSHL )
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('KINOC')
C     !BEG
      call WGET (IS ,CALLER)
C
      NN    = N**2
      NNSHL = NN*NSHL
      NMRR  = NN*MRR
C
      IN( 1) = IS
      IN( 2) = IN( 1)+NNSHL
      IN( 3) = IN( 2)+NMRR
      IN( 4) = IN( 3)+NNSHL
      IN( 5) = IN( 4)+NMRR
      IN( 6) = IN( 5)+NN
      IN( 7) = IN( 6)+N
      IN( 8) = IN( 7)+N
      IN( 9) = IN( 8)+N
      MUX    = IN( 9)+N
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('KINOC')
C
      return
      end
