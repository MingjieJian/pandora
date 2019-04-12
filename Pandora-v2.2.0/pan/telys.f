      subroutine TELYS
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1982 Feb 05
C---- Allocates scratch storage for SAMOS.
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
C
C---- ORIBLOC     as of 2005 Jan 21
      integer     LODLEN,LOD
      dimension   LOD(17)
      common      /ORIBLOC/ LODLEN,LOD
C     This is the "ORION" Data Block index, for the calculation
C     of line source functions in an expanding atmosphere.
C     !DASH
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C     !EJECT
C
      call HI ('TELYS')
C     !BEG
      call WGET (IS,  CALLER)
C
      NN    = N**2
      NNSHL = NN*NSHL
      NMRR  = NN*MRR
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+NNSHL
      IN( 3) = IN( 2)+NMRR
      IN( 4) = IN( 3)+NNSHL
      IN( 5) = IN( 4)+NMRR
      IN( 6) = IN( 5)+NNSHL
      IN( 7) = IN( 6)+NMRR
      IN( 8) = IN( 7)+NNSHL
      IN( 9) = IN( 8)+NMRR
      IN(10) = IN( 9)+N
      IN(11) = IN(10)+LODLEN
C
      IN(12) = IN(11)+NNSHL
      MUX    = IN(12)+NMRR
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('TELYS')
C
      return
      end
