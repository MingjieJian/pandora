      subroutine CULVAN
     $(X,W,IW,XPBL,LL,ICE,XI,A,Z,XNE,VXS,YUL,DPUL,DWUL,DDLUL,FDDLUL,
     $ CDLUL,GTNUL,COPUL,BCUL,DL,K,N,ILFLX,IFDB,MPROM,BKPC,BBC,PBC,
     $ PKPC,PJNU,PRXI,PSIG,PXRD,PZRD,PYRD,IMG,MIK,ISR,KODE,KTNU,
     $ TNUSAV,KISSAV,KILSAV,KNT,DMP1)
C
C     Rudolf Loeser, 1998 Aug 05
C---- Sets up Diana Data Block for the LL'th frequency.
C     !DASH
      save
C     !DASH
      real*8 A, BBC, BCUL, BKPC, CDLUL, COPUL, DDLUL, DL, DLLL, DPUL,
     $       DWUL, FDDLUL, GTNUL, PBC, PJNU, PKPC, PRXI, PSIG, PXRD,
     $       PYRD, PZRD, TNUSAV, VXS, W, X, XI, XNE, XPBL, YUL, Z
      integer ICE, IFDB, ILFLX, IMG, ISR, IW, K, KILSAV, KISSAV, KNT,
     $        KODE, KTNU, LL, LPALL, LPBC, LPJNU, LPKPC, LPPHI, LPPHW,
     $        LPSIG, LPT1, LPT2, LPT3, LPTNU, LPWH, LPWN, LPXLL, MIK,
     $        MPROM, N
      logical DMP1
C     !COM
C---- PERBLOC     as of 2005 Jan 21
      integer     LPDLEN,LPD
      dimension   LPD(15)
      common      /PERBLOC/ LPDLEN,LPD
C     This is the "DIANA" Data Block index, for the calculation of
C     line source functions in a static atmosphere.
      equivalence (LPD(12),LPWH )
      equivalence (LPD( 9),LPPHW)
      equivalence (LPD(13),LPTNU)
      equivalence (LPD( 2),LPPHI)
      equivalence (LPD( 6),LPSIG)
      equivalence (LPD(10),LPALL)
      equivalence (LPD( 3),LPBC )
      equivalence (LPD( 7),LPWN )
      equivalence (LPD( 8),LPKPC)
      equivalence (LPD(11),LPXLL)
      equivalence (LPD( 4),LPJNU)
      equivalence (LPD( 5),LPT1 )
      equivalence (LPD(14),LPT2 )
      equivalence (LPD(15),LPT3 )
C     !DASH
C     !EJECT
      external SHINE, EPIC, CARAVAN, LUNED, FIRE, TANUNA, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               A(K), DL(K), XI(K), VXS(N), XNE(N), GTNUL(N), PBC(N,K),
      dimension A(*), DL(*), XI(*), VXS(*), XNE(*), GTNUL(*), PBC(*),
C
C               BKPC(N,K), DWUL(N), DDLUL(LDL), XPBL(Lpdlen), BBC(N,K),
     $          BKPC(*),   DWUL(*), DDLUL(*),   XPBL(*),      BBC(*),
C
C               COPUL(N), BCUL(N), Z(N), PRXI(N,K), CDLUL(LDL), IMG(N),
     $          COPUL(*), BCUL(*), Z(*), PRXI(*),   CDLUL(*),   IMG(*),
C
C               PKPC(N,K), PSIG(N,K), PJNU(N,K), KISSAV(KM), FDDLUL(N),
     $          PKPC(*),   PSIG(*),   PJNU(*),   KISSAV(*),  FDDLUL(*),
C
C               DPUL(N,LDL), TNUSAV(N,KM), KILSAV(KM), PXRD(N,K),
     $          DPUL(*),     TNUSAV(*),    KILSAV(*),  PXRD(*),
C
C               PYRD(N,K), PZRD(N,K)
     $          PYRD(*),   PZRD(*)
C
      dimension DLLL(1)
C     !EJECT
C
      call HI ('CULVAN')
C     !BEG
C---- Set up Delta-Lambda, frequency and integration weight
      DLLL(1) = DL(LL)
      XPBL(LPXLL) = XI(LL)
      XPBL(LPALL) = A(LL)
      if(DMP1) then
        call LUNED      (LL, XPBL(LPXLL), DLLL, XPBL(LPALL))
      end if
C---- Set up line background, and associated P.R.D. data
      call SHINE        (LL, K, N, ICE, IFDB, BCUL, COPUL, XPBL(LPBC),
     $                   XPBL(LPKPC), XPBL(LPJNU), XPBL(LPSIG),
     $                   XPBL(LPT1), XPBL(LPT2), XPBL(LPT3), BKPC, BBC,
     $                   PBC, PKPC, PJNU, PRXI, PSIG, PXRD, PZRD, PYRD)
C---- Set up PHI
      call EPIC         (N, DLLL, VXS, XNE, DPUL, DWUL, DDLUL, FDDLUL,
     $                   CDLUL, XPBL(LPPHI), XPBL(LPPHW), W, IW, LL,
     $                   MPROM, DMP1)
C---- Set up TNU
      call CARAVAN      (X, W, LL, N, XPBL(LPXLL), DLLL, XPBL(LPPHI),
     $                   COPUL, GTNUL, XPBL(LPTNU), IMG, MIK, DMP1)
      if(MIK.le.0) then
        if(ICE.eq.0) then
C----     Set up weight matrices
          call FIRE     (X, W, IW, LL, N, DLLL, XPBL(LPXLL), YUL, XNE,
     $                   DPUL, DWUL, DDLUL, FDDLUL, CDLUL, COPUL,
     $                   GTNUL, Z, XPBL(LPTNU), XPBL(LPPHI),
     $                   XPBL(LPWN), XPBL(LPWH), ISR, MIK, IMG, ILFLX,
     $                   MPROM, DMP1)
          if(KTNU.gt.0) then
C----       Save data for TNU-analysis
            call TANUNA (LL, N, XPBL(LPTNU), TNUSAV, KISSAV, KILSAV,
     $                   KNT)
          end if
        else
          ISR = 1
        end if
      end if
C     !END
      call BYE ('CULVAN')
C
      return
      end
