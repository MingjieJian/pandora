      subroutine RUFF
     $(LU)
C
C     Rudolf Loeser, 2002 Mar 29
C---- Initializes a restart data file.
C     (This is version 2 of RUFF.)
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer IOMX, IONST, IOVER, IQAN1, KAMB, KPTM, LU, N, NL, NONC,
     $        NT
      character QELSM*8, qummy*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ( 5),NT )
C
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
      equivalence (QZQ(  2),QELSM)
      equivalence (KZQ( 56),IONST)
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
      equivalence (LEST(32),KAMB )
      equivalence (LEST(29),NONC )
C     !EJECT
C---- VERSION     as of 2006 May 16
      real*8      VERSION
      integer     NVDSCR
      character   VDSCRPT*63
      dimension   VDSCRPT(45)
      common      /VERSION1/ VERSION
      common      /VERSION2/ NVDSCR
      common      /VERSION3/ VDSCRPT
C     Identifier and description of this version of PANDORA.
C     (Values set by subroutine AARDVRK.)
C     .
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
      equivalence (IQQ(272),IQAN1)
C
C---- HEADER      as of 1984 Apr 23
      character   HEAD*80
      common      /HEADER/ HEAD
C     Copy of the "HEADER" line for this run.
C     .
C     !DASH
C     !EJECT
      external RUN_DATA, ZEUS, HI, BYE
C
      call HI ('RUFF')
C     !BEG
C
      rewind LU
C
      write (LU,100) IOVER,IOMX,VERSION
  100 format('> from Overall Iteration',I3,' of',I3,32X,'Pandora',F7.3)
C
      call ZEUS     (KAMB, IQAN1, KPTM)
      call RUN_DATA (LU, 1, N, NL, NT, NONC, KPTM, QELSM, IONST,
     $               qummy, qummy, dummy)
C
      write (LU,101) HEAD(3:)
  101 format('> ',A)
C
      write (LU,102)
  102 format('> -------.',7('---------.'))
C     !END
      call BYE ('RUFF')
C
      return
      end
