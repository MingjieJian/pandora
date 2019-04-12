      subroutine CALEDON
     $(LL,K,XI,A,DLLL,N,NSHL,NRPMX,MRR,IND,ICE,TNUN,LNRM,BCUL,COPUL,
     $ DPUL,DWUL,MPROM,XNE,DDLUL,FDDLUL,CDLUL,LDL,CSHL,WSHL,PHISHL,
     $ WNSHL,WHSHL,ILFLX,SHLKOD,CDSK,WDSK,PHIDSK,WNDSK,WHDSK,DSKKOD,
     $ XOBL,IFDB,BKPC,BBC,PBC,PKPC,PJNU,PRXI,PSIG,PXRD,PZRD,PYRD,
     $ W,IW)
C
C     Rudolf Loeser, 1983 Mar 02
C---- Sets up ORION Data Blocks, for HERON.
C     !DASH
      save
C     !DASH
      real*8 A, BBC, BCUL, BKPC, CDLUL, CDSK, COPUL, CSHL, DDLUL, DLLL,
     $       DPUL, DSKKOD, DWUL, FDDLUL, PBC, PHIDSK, PHISHL, PJNU,
     $       PKPC, PRXI, PSIG, PXRD, PYRD, PZRD, SHLKOD, TNUN, W, WDSK,
     $       WHDSK, WHSHL, WNDSK, WNSHL, WSHL, XI, XNE, XOBL
      integer ICE, IFDB, ILFLX, IN, IND, IS, IVXX, IW, K, LDL, LL, LNRM,
     $        MOX, MPROM, MRR, N, NRPMX, NSHL
C     !DASH
      external ELAF, CYMRY, EIRE, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               NP = (N-2)/NTAN + 1
C
C               BBC(N,K), BCUL(N), COPUL(N), PHISHL(N,N,NSHL), TNUN(N),
      dimension BBC(*),   BCUL(*), COPUL(*), PHISHL(*),        TNUN(*),
C
C               DPUL(N,LDL), DWUL(N), DDLUL(LDL), CDLUL(LDL), PBC(N,K),
     $          DPUL(*),     DWUL(*), DDLUL(*),   CDLUL(*),   PBC(*),
C
C               CSHL(N,NP), PSIG(N,K), CDSK(N,MRR), XNE(N),  PJNU(N,K),
     $          CSHL(*),    PSIG(*),   CDSK(*),     XNE(*),  PJNU(*),
C
C               WNSHL(N,N,NSHL), WHSHL(N,N,NSHL), WNDSK(N,N,MRR), A(K),
     $          WNSHL(*),        WHSHL(*),        WNDSK(*),       A(*),
C
C               WSHL(N,NP), PHIDSK(N,N,MRR), XOBL(Lodlen), DSKKOD(MRR),
     $          WSHL(*),    PHIDSK(*),       XOBL(*),      DSKKOD(*),
C
C               WHDSK(N,N,MRR), PRXI(N,K), FDDLUL(N), BKPC(N,K), XI(K),
     $          WHDSK(*),       PRXI(*),   FDDLUL(*), BKPC(*),   XI(*),
C
C               PKPC(N,K), DLLL(1), SHLKOD(NSHL), PXRD(N,K), PYRD(N,K),
     $          PKPC(*),   DLLL(*), SHLKOD(*),    PXRD(*),   PYRD(*),
C
C               WDSK(N,MRR), PZRD(N,K)
     $          WDSK(*),     PZRD(*)
C
      dimension   IN(1)
      equivalence
     $(IN( 1),IVXX  )
C     !EJECT
C
      call HI ('CALEDON')
C     !BEG
C     (Get, and allocate, W allotment)
      call ELAF  (IN, IS, MOX, 'CALEDON')
C
      call CYMRY (LL, K, XI, A, DLLL, N, NSHL, NRPMX, IND, ICE, TNUN,
     $            LNRM, BCUL, COPUL, DPUL, DWUL, MPROM, XNE, DDLUL,
     $            FDDLUL, CDLUL, LDL, CSHL, WSHL, PHISHL, WNSHL,
     $            SHLKOD, WHSHL, ILFLX, XOBL, IFDB, BKPC, BBC, PBC,
     $            PKPC, PJNU, PRXI, PSIG, PXRD, PZRD, PYRD, W(IVXX),
     $            W, IW)
C
      call EIRE  (LL, K, XI, A, DLLL, N, MRR, IND, ICE, TNUN, LNRM,
     $            BCUL, COPUL, DPUL, DWUL, MPROM, XNE, DDLUL, FDDLUL,
     $            CDLUL, LDL, CDSK, WDSK, PHIDSK, WNDSK, DSKKOD, WHDSK,
     $            ILFLX, XOBL, IFDB, BKPC, BBC, PBC, PKPC, PJNU, PRXI,
     $            PSIG, PXRD, PZRD, PYRD, W(IVXX), W, IW)
C
C     (Give back W allotment)
      call WGIVE (W, 'CALEDON')
C     !END
      call BYE ('CALEDON')
C
      return
      end
