      subroutine TYNE
     $(X,IX,W,IW)
C     Rudolf Loeser, 1980 Jul 31
C---- Controls "ITER":
C     PANDORA's iterative calculations.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IOMX, IOVER, IW, IX, MO
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(  8),IOMX )
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
      equivalence (LEST( 2),IOVER)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
C     !EJECT
      external NOW, KISS, BRAY, WYE, MERSEY, BRUE, YARE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      call HI ('TYNE')
C     !BEG
      do 100 IOVER = 1,IOMX
        call NOW    (MO)
        call KISS   (X)
C----   TAUK processing, if needed
        call BRAY   (X, IX, W, IW)
C----   Continuum-related processing
        call WYE    (X, IX, W, IW)
C----   Lines processing
        call MERSEY (X, IX, W, IW)
C----   HSL-iterations
        call BRUE   (X, IX, W, IW)
C----   Restart Data
        call YARE   (X, IX, W, IW)
  100 continue
      IOVER = 0
C     !END
      call BYE ('TYNE')
C
      return
      end
