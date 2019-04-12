      subroutine MIAS
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1990 Nov 30
C---- Controls calculation of Upper-Level Charge Exchange
C     terms CXX and CXXP.
C     !DASH
      save
C     !DASH
      real*8 CCHX, W, X
      integer IARR, ICXDP, IG, IN, IQCXD, IQCXP, IS, IW, IWRK, IWS, IX,
     $        IZL, JJCXP, JJCXX, JJLCX, JJLRQ, JJNPQ, JJTE, JJZ, JN,
     $        LSTXL, LUP, MCXK, MOX, MUX, N, NL, NO
      logical DUMP
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(217),JJCXX)
      equivalence (IZOQ(216),JJCXP)
      equivalence (IZOQ( 37),JJZ  )
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  9),JJNPQ)
      equivalence (JZOQ( 10),JJLRQ)
      equivalence (JZOQ( 11),JJLCX)
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
      equivalence (RZQ( 99),CCHX )
      equivalence (KZQ(116),ICXDP)
C     !EJECT
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(55),MCXK )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
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
      equivalence (IQQ(276),IQCXP)
      equivalence (IQQ(277),IQCXD)
C     !DASH
      external MOTO, ZEUS, KILOSA, NIAS, WGIVE, IGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IG    ),(IN( 2),IARR ),(IN( 3),IZL   ),(IN( 4),IWRK  )
C
      dimension JN(1)
      equivalence
     $(JN( 1),LSTXL )
C     !EJECT
C
      call HI ('MIAS')
C     !BEG
      if(MCXK.gt.0) then
C       (Get, and allocate, W & IW allotments)
        call MOTO   (IN, IS,  MOX, 'MIAS')
        call NIAS   (JN, IWS, MUX, 'MIAS')
C
        DUMP = (IQCXD.gt.0).and.(ICXDP.ge.1).and.(ICXDP.le.N)
        call ZEUS   (NO, IQCXP, LUP)
        call KILOSA (N, NL, MCXK, IX(JJLCX), IX(JJNPQ), IX(JJLRQ),
     $               X(JJTE), X(JJCXX), X(JJCXP), CCHX, ICXDP, W(IG),
     $               IW(LSTXL), W(IARR), X(JJZ), W(IZL), LUP, DUMP,
     $               W(IWRK))
C
C       (Give back W & IW allotments)
        call WGIVE  (W,  'MIAS')
        call IGIVE  (IW, 'MIAS')
      end if
C     !END
      call BYE ('MIAS')
C
      return
      end
