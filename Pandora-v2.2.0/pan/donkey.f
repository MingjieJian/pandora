      subroutine DONKEY
     $(GO,UPJNU)
C
C     Rudolf Loeser, 2002 Apr 04
C---- Computes processing control switches for YARE.
C     (This is version 2 of DONKEY.)
C     !DASH
      save
C     !DASH
      integer IOMX, IOVER, IQIRC, KNZGM, NONC
      logical DOIT, GO, LASTOVR, UPJNU
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
      equivalence (LEST(29),NONC )
      equivalence (LEST(26),KNZGM)
C
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ( 41),IQIRC)
C     !DASH
C     !EJECT
      external HI, BYE
C
      call HI ('DONKEY')
C     !BEG
      LASTOVR = IOVER.eq.IOMX
      DOIT    = (NONC.gt.0).and.(KNZGM.gt.0)
      UPJNU   = LASTOVR.and.DOIT
C
      GO = LASTOVR.or.(IQIRC.gt.0)
C     !END
      call BYE ('DONKEY')
C
      return
      end
