      subroutine MAURON
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1983 Mar 02
C---- Allocates scratch storage for HERON.
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
C     !EJECT
C
      call HI ('MAURON')
C     !BEG
      call WGET (IS ,CALLER)
C
      NN    = N**2
      NNSHL = NN*NSHL
      NMRR  = NN*MRR
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NNSHL
      IN( 3) = IN( 2)+NNSHL
      IN( 4) = IN( 3)+NSHL
      IN( 5) = IN( 4)+NMRR
      IN( 6) = IN( 5)+NMRR
      IN( 7) = IN( 6)+MRR
      IN( 8) = IN( 7)+NMRR
      MUX    = IN( 8)+NNSHL
C
      call WLCK (MUX,CALLER)
C     !END
      call BYE ('MAURON')
C
      return
      end
