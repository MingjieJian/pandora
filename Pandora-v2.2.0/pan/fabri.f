      subroutine FABRI
     $(NO,TE,TR,Z,ZLOG)
C
C     Rudolf Loeser, 1991 Jul 31
C---- Prints various things, at the end of PHASE1.
C     !DASH
      save
C     !DASH
      real*8 TE, TR, Z, ZLOG
      integer IOMX, IOVER, IQTRP, N, NO, NSL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(40),NSL)
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
C     !EJECT
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
      equivalence (IQQ(291),IQTRP)
C     !DASH
      external NERLI, KALPE, HI, BYE
C
C               TE(N), TR(N,NSL), Z(N), ZLOG(N)
      dimension TE(*), TR(*),     Z(*), ZLOG(*)
C
      call HI ('FABRI')
C     !BEG
      if((IOMX.eq.IOVER).and.(IQTRP.gt.0).and.(NO.gt.0)) then
        call NERLI (NO,N,NSL,TE,TR)
        call KALPE (NO,TE,TR,NSL,Z,N,ZLOG,'effective')
      end if
C     !END
      call BYE ('FABRI')
C
      return
      end
