      subroutine KNOT
     $(W,KKU,XKX,GKX,AKX,BDI,XINK,FINK,Z,XLCR,XICR,XNU,EP1,EP2,CP,U,
     $ EMUX,V,TNU,F1,TNUL,RNDT,DNRT,DNRTC,CNXP,XLB,SP,XLP,D,ERT,TK,
     $ IETA,KASE)
C
C     Rudolf Loeser, 1975 Sep 29
C---- Computes auxiliary functions for Lyman calculations.
C     (This is version 4 of KNOT.)
C     !DASH
      save
C     !DASH
      real*8 AKX, BDI, CNXP, CP, D, DNRT, DNRTC, EMUX, EP1, EP2, ERT,
     $       EXLYM, F1, FINK, GKX, RNDT, SP, TGLYM, TK, TNU, TNUL, U, V,
     $       W, XICR, XINK, XKX, XLB, XLCR, XLP, XNU, XNUK, Z
      integer IDNRT, IEMUL, IETA, IGL, IN, IQCCR, IQINC, IRAT, IS, IVL,
     $        IXL, KASE, KKU, KOLEV, LNLIM, MO, MOX, N, NCR, NL
      logical KOOL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(32),NCR)
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
      equivalence (KZQ( 33),KOLEV)
      equivalence (RZQ(  9),XNUK )
      equivalence (KZQ( 16),LNLIM)
      equivalence (RZQ( 12),EXLYM)
      equivalence (RZQ( 13),TGLYM)
      equivalence (KZQ(148),IDNRT)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
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
      equivalence (IQQ( 51),IQINC)
C     !DASH
      external LEADER, SQUEEK, FERN, LIMPID, DESERT, CHOOSE, WGIVE,
     $         HI, BYE
C
      dimension W(*)
C
C               XKX(KKX), GKX(KKX), TK(N), BDI(N,NL), XINK(INK), F1(N),
      dimension XKX(*),   GKX(*),   TK(*), BDI(*),    XINK(*),   F1(*),
C
C               SP(N,KKX), XLCR(NCR), XICR(NCR), XNU(NSL), TNUL(N,NCR),
     $          SP(*),     XLCR(*),   XICR(*),   XNU(*),   TNUL(*),
C
C               DNRT(N), EP2(N), XLP(N), RNDT(N), V(N,KKX), TNU(N,KKX),
     $          DNRT(*), EP2(*), XLP(*), RNDT(*), V(*),     TNU(*),
C
C               Z(N), ERT(N), U(N), EP1(N), CP(NSL), CNXP(N,KKX), D(N),
     $          Z(*), ERT(*), U(*), EP1(*), CP(*),   CNXP(*),     D(*),
C
C               DNRTC(N), FINK(INK), XLB(N,KKX), AKX(KKX), EMUX(N,KKX)
     $          DNRTC(*), FINK(*),   XLB(*),     AKX(*),   EMUX(*)
C
      dimension IN(5)
      equivalence
     $(IN( 1),IRAT  ),(IN( 2),IXL   ),(IN( 3),IGL   ),(IN( 4),IVL   ),
     $(IN( 5),IEMUL )
C     !EJECT
C
      call HI ('KNOT')
C     !BEG
C     (Get, and allocate, W allotment)
      call LEADER (IN, IS, MOX, 'KNOT')
C
C---- Compute V
      call SQUEEK (KKU, EMUX, BDI, N, NL, V, KOLEV)
C---- Compute RNDT
      KOOL = ((IQCCR.gt.0).and.(MO.gt.0))
      call FERN   (XNUK, TNU, N, KKU, XKX, W(IRAT), XINK, FINK, AKX,
     $             Z, GKX, V, F1, IQINC, NCR, XLCR, XICR, W(IXL),
     $             W(IGL), W(IVL), U, W(IEMUL), BDI, NL, KOLEV, TNUL,
     $             RNDT, DNRT, DNRTC, KOOL, XNU, CNXP, CP, IDNRT)
C---- Compute XLP and D
      call LIMPID (AKX, GKX, V, XLB, SP, XKX, EMUX, F1, N, KKU, XNUK,
     $             XLP, D)
C---- Compute ERT
      call DESERT (N, RNDT, F1, EP2, EP1, XLP, D, ERT)
C---- Find ETA and KASE
      call CHOOSE (TK, EP1, N, LNLIM, EXLYM, TGLYM, IETA, KASE)
C
C     (Give back W allotment)
      call WGIVE  (W, 'KNOT')
C     !END
      call BYE ('KNOT')
C
      return
      end
