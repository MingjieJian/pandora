      subroutine BITTERN
     $(X,W,IW,LL,K,XI,A,DLLL,BCUL,GTNUL,COPUL,DPUL,DWUL,XNE,DDLUL,
     $ FDDLUL,CDLUL,LDL,XMU,CMU,WMU,VXS,WVLUL,INDS,ICE,ILFLX,IFDB,
     $ MPROM,Y,MOVING,TNUN,LNRM,XOBL,IMG,BKPC,BBC,PBC,PKPC,PJNU,PRXI,
     $ PSIG,PXRD,PZRD,PYRD,DUMP)
C
C     Rudolf Loeser, 1983 Mar 01
C---- Controls preparation of updated ORION Data Blocks, for the LL'th
C     frequency.
C
C     These Blocks are initialized by "MARANT". In the case of PRD,
C     PHIW, WN and WH will already have been updated as part of the
C     PRD Continuum Jnu calculation, by "ABALLAC" via "SALAMIS".
C     !DASH
      save
C     !DASH
      real*8 A, BBC, BCUL, BKPC, CDLUL, CMU, COPUL, DDLUL, DLLL, DPUL,
     $       DWUL, FDDLUL, GTNUL, PBC, PJNU, PKPC, PRXI, PSIG, PXRD,
     $       PYRD, PZRD, TNUN, VXS, W, WMU, WVLUL, X, XI, XMU, XNE,
     $       XOBL, Y
      integer ICE, IFDB, ILFLX, IMG, IN, IND, INDS, IPHI, IRAYL, IS, IW,
     $        IWH, IWN, K, LDL, LG, LL, LNRM, MOX, MPROM, N
      logical DUMP, MOVING
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(34),LG )
C     !DASH
C     !EJECT
      external MARGON, CANUTE, ONE1, GORMUND, WGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XOBL(Lodlen), DLLL(1), PSIG(N,K), VXS(N), XNE(N), A(K),
      dimension XOBL(*),      DLLL(*), PSIG(*),   VXS(*), XNE(*), A(*),
C
C               GTNUL(N), COPUL(N), DPUL(N,LDL), BKPC(N,K), CDLUL(LDL),
     $          GTNUL(*), COPUL(*), DPUL(*),     BKPC(*),   CDLUL(*),
C
C               FDDLUL(N), PKPC(N,K), PJNU(N,K), DDLUL(LDL), PRXI(N,K),
     $          FDDLUL(*), PKPC(*),   PJNU(*),   DDLUL(*),   PRXI(*),
C
C               BCUL(N), DWUL(N), PBC(N,K), BBC(N,K), XMU(LG), TNUN(N),
     $          BCUL(*), DWUL(*), PBC(*),   BBC(*),   XMU(*),  TNUN(*),
C
C               WMU(LG), CMU(LG), IMG(N), PXRD(N,K), PYRD(N,K), XI(K),
     $          WMU(*),  CMU(*),  IMG(*), PXRD(*),   PYRD(*),   XI(*),
C
C               PZRD(N,K)
     $          PZRD(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IPHI  ),(IN( 2),IWN   ),(IN( 3),IRAYL ),(IN( 4),IWH   )
C
      call HI ('BITTERN')
C     !BEG
C     (Get, and allocate, W allotment)
      call MARGON    (IN, IS, MOX, 'BITTERN')
      if(ICE.eq.0) then
C----   Set up PHI, matrices, and RAYL
        IND = INDS
        call CANUTE  (X, W, IW, N, VXS, GTNUL, COPUL, DPUL, DWUL, XNE,
     $                MPROM, DDLUL, FDDLUL, CDLUL, LDL, XMU, WVLUL,
     $                DLLL, LL, LG, IND, W(IPHI), W(IWN), W(IWH),
     $                ILFLX, Y, MOVING, W(IRAYL), IMG, DUMP)
      else
C----   Set up RAYL only
        call ONE1    (W(IRAYL), LG)
      end if
C---- Update data blocks
      IND = INDS
      call GORMUND   (LL, K, XI, A, DLLL, N, LG, IND, ICE, TNUN, LNRM,
     $                BCUL, COPUL, DPUL, DWUL, MPROM, XNE, DDLUL,
     $                FDDLUL, CDLUL, LDL, CMU, WMU, W(IPHI), W(IWN),
     $                W(IRAYL), XOBL, W(IWH), ILFLX, IFDB, BKPC, BBC,
     $                PBC, PKPC, PJNU, PRXI, PSIG, PXRD, PZRD, PYRD,
     $                W, IW)
C
C     (Give baxk W allotment)
      call WGIVE     (W, 'BITTERN')
C     !END
      call BYE ('BITTERN')
C
      return
      end
