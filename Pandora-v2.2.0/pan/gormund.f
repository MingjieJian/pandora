      subroutine GORMUND
     $(LL,K,XI,A,DLUL,N,LG,IND,ICE,TNUN,LNRM,BCUL,COPUL,DPUL,DWUL,
     $ MPROM,XNE,DDLUL,FDDLUL,CDLUL,LDL,CMU,WMU,PHIA,WNA,RAYL,XOBL,
     $ WHA,ILFLX,IFDB,BKPC,BBC,PBC,PKPC,PJNU,PRXI,PSIG,PXRD,PZRD,
     $ PYRD,W,IW)
C
C     Rudolf Loeser, 1983 Mar 02
C---- Sets up ORION Data Blocks, for BITTERN.
C     !DASH
      save
C     !DASH
      real*8 A, BBC, BCUL, BKPC, CDLUL, CMU, COPUL, DDLUL, DLUL, DPUL,
     $       DWUL, FDDLUL, PBC, PHIA, PJNU, PKPC, PRXI, PSIG, PXRD,
     $       PYRD, PZRD, RAYL, TNUN, W, WHA, WMU, WNA, XI, XNE, XOBL
      integer ICE, IFDB, ILFLX, IN, IND, IS, IVXX, IW, K, LDL, LG, LL,
     $        LNRM, MOX, MPROM, N
C     !DASH
      external ELAF, BREIZ, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               XI(K), XNE(N), DLUL(1), TNUN(N), BCUL(N), PHIA(N,N,LG),
      dimension XI(*), XNE(*), DLUL(*), TNUN(*), BCUL(*), PHIA(*),
C
C               COPUL(N), DPUL(N,LDL), PSIG(N,K), DDLUL(LDL), RAYL(LG),
     $          COPUL(*), DPUL(*),     PSIG(*),   DDLUL(*),   RAYL(*),
C
C               CDLUL(LDL), CMU(LG), PRXI(N,K), XOBL(Lodlen), PBC(N,K),
     $          CDLUL(*),   CMU(*),  PRXI(*),   XOBL(*),      PBC(*),
C
C               WNA(N,N,LG), FDDLUL(N), BBC(N,K), PJNU(N,L), BKPC(N,K),
     $          WNA(*),      FDDLUL(*), BBC(*),   PJNU(*),   BKPC(*),
C
C               PKPC(N,K), WHA(N,N,LG), DWUL(N), PXRD(N,K), PYRD(N,K),
     $          PKPC(*),   WHA(*),      DWUL(*), PXRD(*),   PYRD(*),
C
C               WMU(LG), A(K), PZRD(N,K)
     $          WMU(*),  A(*), PZRD(*)
C
      dimension   IN(1)
      equivalence
     $(IN( 1),IVXX  )
C     !EJECT
C
      call HI ('GORMUND')
C     !BEG
C     (Get, and allocate, W allotment)
      call ELAF  (IN, IS, MOX, 'GORMUND')
C
      call BREIZ (LL, K, XI, A, DLUL, N, LG, IND, ICE, TNUN, LNRM,
     $            BCUL, COPUL, DPUL, DWUL, MPROM, XNE, DDLUL, FDDLUL,
     $            CDLUL, LDL, CMU, WMU, PHIA, WNA, RAYL, WHA, ILFLX,
     $            XOBL, IFDB, BKPC, BBC, PBC, PKPC, PJNU, PRXI, PSIG,
     $            PXRD, PZRD, PYRD, W(IVXX), W, IW)
C
C     (Give back W allotment)
      call WGIVE (W, 'GORMUND')
C     !END
      call BYE ('GORMUND')
C
      return
      end
