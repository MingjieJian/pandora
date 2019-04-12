      subroutine LOWELL
     $(N,NL,KK,CP,AK,GK,XK,EMUX,XJIK,XNU,RLC,RKC,DNRTC,NCR)
C
C     Rudolf Loeser, 1979 Oct 19
C---- Computes cooling rates data for ROPE.
C     !DASH
      save
C     !DASH
      real*8 AK, CP, DNRTC, EMUX, GK, RKC, RLC, XJIK, XK, XNU
      integer IHSLT, IOMX, IOVER, IQCCR, ITHSL, KK, KOLEV, LITER, LYMIT,
     $        N, NCR, NL
      logical KOOL, LAST
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
      equivalence (KZQ( 20),IHSLT)
      equivalence (KZQ( 19),LYMIT)
      equivalence (KZQ( 33),KOLEV)
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
      equivalence (LEST(19),ITHSL)
      equivalence (LEST(24),LITER)
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
      equivalence (IQQ(133),IQCCR)
C     !DASH
      external BAKE, MALE, HI, BYE
C
C               RLC(N,NL), RKC(N,NL), DNRTC(N), CP(NSL+1), XJIK(N,KKX),
      dimension RLC(N,*),  RKC(N,*),  DNRTC(*), CP(*),     XJIK(*),
C
C               GK(KKX), EMUX(N,KKX), XNU(NSL), XK(KKX), AK(KKX)
     $          GK(*),   EMUX(*),     XNU(*),   XK(*),   AK(*)
C
      data KOOL /.true./
C
      call HI ('LOWELL')
C     !BEG
      LAST = ((IOVER*ITHSL*LITER).eq.(IOMX*IHSLT*LYMIT))
C
      if(LAST.and.(IQCCR.gt.0)) then
        call BAKE (CP, RKC(1,KOLEV), KK, AK, GK, XK, XJIK, N, KOLEV,
     $             DNRTC, NCR, KOOL, XNU)
        call MALE (CP, RLC(1,KOLEV), KK, AK, GK, XK, XJIK, N, KOLEV,
     $             EMUX,       KOOL, XNU)
      end if
C     !END
      call BYE ('LOWELL')
C
      return
      end
