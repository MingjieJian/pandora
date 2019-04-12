      subroutine CARRON
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1975 May 21
C---- Drives iterations summaries.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IMAX, IMIN, IW, IX, KNEGA, NPROG
C     !COM
C---- IBIS        as of 2003 May 30
      integer     MXIBIS,NTMX,IBADR,IBKOD,IBNIT,IBLEN,IBIN1,IBIN2,
     $            IBITE,IBLIT,IBITH,IBIOV,NITS,NIBIS
      character   IBSRC*10, IBNAM*10
      parameter   (MXIBIS=1000)
      parameter   (NTMX=14)
C     (Remember to recompile all users when changing MXIBIS or NTMX)
      dimension   IBADR(MXIBIS), IBKOD(MXIBIS), IBNIT(MXIBIS),
     $            IBLEN(MXIBIS), IBIN1(MXIBIS), IBIN2(MXIBIS),
     $            IBITE(MXIBIS), IBLIT(MXIBIS), IBITH(MXIBIS),
     $            IBIOV(MXIBIS), IBSRC(MXIBIS), IBNAM(MXIBIS),
     $            NITS(NTMX)
      common      /IBIS1/ NIBIS, IBADR, IBKOD, IBNIT, IBLEN, IBIN1,
     $                           IBIN2, IBITE, IBLIT, IBITH, IBIOV
      common      /IBIS2/        IBSRC, IBNAM
      common      /IBIS3/ NITS
C     Control information for iterative summary data.
C
C         NITS = counts of iteration summary data records for:
C
C      1: TAU(IU,IL),    2: CHECK(L)       3: RHO(IU,IL)
C      4: RK(KOLEV)      5: ND(L)          6: RHOWT(IU,IL)
C      7: BD(KOLEV)      8: NE             9: CHI(IU,IL)
C     10: Z             11: S(IU,IL)      12: NH
C     13: TDST          14: NK
C
C---- SELGI       as of 1999 Sep 13
      integer     KNFMX
      parameter   (KNFMX=50)
C     (Remember to recompile all users when changing KNFMX)
      integer     KNTF,INF1,INF2,INF3,INF4
      real*8      FUJJ,FVAL
      dimension   INF1(KNFMX),INF2(KNFMX),INF3(KNFMX),INF4(KNFMX),
     $            FUJJ(KNFMX),FVAL(KNFMX)
      common      /SELGI1/ KNTF,INF1,INF2,INF3,INF4
      common      /SELGI2/ FUJJ,FVAL
C     Saves B-ratios computation fudging data, for later printing.
C     .
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
      equivalence (LEST(56),KNEGA)
C     !DASH
      external LOGIN, JEWEL, MINMAXI, LOGOUT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      data NPROG /22/
C
      call HI ('CARRON')
C     !BEG
      call MINMAXI   (NITS,1,NTMX,IMIN,IMAX)
C
      if((NITS(IMAX).gt.0).or.(KNTF.gt.0).or.(KNEGA.gt.0)) then
        call LOGIN   (NPROG)
        call JEWEL   (X, W)
        call LOGOUT  (NPROG)
      end if
C     !END
      call BYE ('CARRON')
C
      return
      end
