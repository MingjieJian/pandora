      subroutine HERON
     $(X,W,IW,LL,K,XI,A,DLLL,BCUL,GTNUL,COPUL,DPUL,DWUL,XNE,DDLUL,
     $ FDDLUL,CDLUL,LDL,XSHL,CODSRW,Z,R1N,XDSK,EMDSK,CSHL,CDSK,WSHL,
     $ WDSK,VXS,WVLUL,INDS,ICE,ILFLX,IFDB,MPROM,Y,MOVING,TNUN,LNRM,
     $ XOBL,IMG,BKPC,BBC,PBC,PKPC,PJNU,PRXI,PSIG,PXRD,PZRD,PYRD,
     $ DUMP)
C
C     Rudolf Loeser, 1983 Mar 02
C---- Controls preparation of updated ORION Data Blocks, for the LL'th
C     frequency.
C
C     These Blocks are initialized by "RUFFIN". In the case of PRD,
C     PHIW, WN and WH will already have been updated as part of the
C     PRD Continuum Jnu calculation, by "HORSA" via "SAMOS".
C     (This is version 2 of HERON.)
C     !DASH
      save
C     !DASH
      real*8 A, BBC, BCUL, BKPC, CDLUL, CDSK, CODSRW, COPUL, CSHL,
     $       DDLUL, DLLL, DPUL, DWUL, EMDSK, FDDLUL, GTNUL, PBC, PJNU,
     $       PKPC, PRXI, PSIG, PXRD, PYRD, PZRD, R1N, TNUN, VXS, W,
     $       WDSK, WSHL, WVLUL, X, XDSK, XI, XNE, XOBL, XSHL, Y, Z
      integer ICE, IFDB, ILFLX, ILRYD, ILRYS, IMG, IN, IND, INDS, IPHID,
     $        IPHIS, IS, IW, IWHD, IWHS, IWND, IWNS, K, LDL, LL, LNRM,
     $        MOX, MPROM, MRR, N, NRPMX, NSHL
      logical DUMP, MOVING
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(15),MRR)
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
      equivalence (LEST( 4),NSHL )
      equivalence (LEST( 8),NRPMX)
C     !DASH
C     !EJECT
      external MAURON, ANGARAD, ONE1, CALEDON, WGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               NRM = 2*N + 5
C
C               XOBL(Lodlen), DLLL(1), A(K), XNE(N), Z(N), XDSK(N,MRR),
      dimension XOBL(*),      DLLL(*), A(*), XNE(*), Z(*), XDSK(*),
C
C               GTNUL(N), WSHL(N,NSHL), DPUL(N,LDL), FDDLUL(N), VXS(N),
     $          GTNUL(*), WSHL(*),      DPUL(*),     FDDLUL(*), VXS(*),
C
C               CDLUL(LDL), XSHL(NRM,NSHL), CODSRW(NSHL), EMDSK(N,MRR),
     $          CDLUL(*),   XSHL(*),        CODSRW(*),    EMDSK(*),
C
C               BCUL(N), CSHL(N,NSHL), PBC(N,K), PSIG(N,K), DDLUL(LDL),
     $          BCUL(*), CSHL(*),      PBC(*),   PSIG(*),   DDLUL(*),
C
C               CDSK(N,MRR), WDSK(N,MRR), BBC(N,K), DWUL(N), PJNU(N,K),
     $          CDSK(*),     WDSK(*),     BBC(*),   DWUL(*), PJNU(*),
C
C               BKPC(N,K), COPUL(N), XI(K), IMG(N), PRXI(N,K), TNUN(N),
     $          BKPC(*),   COPUL(*), XI(*), IMG(*), PRXI(*),   TNUN(*),
C
C               PKPC(N,K), PXRD(N,K), PYRD(N,K), PZRD(N,K)
     $          PKPC(*),   PXRD(*),   PYRD(*),   PZRD(*)
C
      dimension IN(9)
      equivalence
     $(IN( 1),IPHIS ),(IN( 2),IWNS  ),(IN( 3),ILRYS ),(IN( 4),IPHID ),
     $(IN( 5),IWND  ),(IN( 6),ILRYD ),(IN( 7),IWHD  ),(IN( 8),IWHS  )
C     !EJECT
C
      call HI ('HERON')
C     !BEG
C     (Get, and allocate, W allotmant)
      call MAURON    (IN, IS, MOX, 'HERON')
C
      if(ICE.eq.0) then
C----   Set up PHI, WN, WH and LRAY
        IND = INDS
        call ANGARAD (N, NSHL, NRPMX, CODSRW, MRR, W(IPHIS), W(IPHID),
     $                W(IWNS), W(IWND), W(ILRYS), W(ILRYD), VXS, GTNUL,
     $                COPUL, DPUL, DWUL, MPROM, XNE, DDLUL, FDDLUL,
     $                CDLUL, LDL, XSHL, XDSK, Z, R1N, EMDSK, WVLUL,
     $                DLLL, LL, IND, W(IWHS), W(IWHD), ILFLX, Y,
     $                MOVING, IMG, W, IW, DUMP)
      else
C----   Set up RAYL only
        call ONE1    (W(ILRYS), NSHL)
        call ONE1    (W(ILRYD), MRR )
      end if
C
C---- Update data blocks
      IND = INDS
      call CALEDON   (LL, K, XI, A, DLLL, N, NSHL, NRPMX, MRR, IND,
     $                ICE, TNUN, LNRM, BCUL, COPUL, DPUL, DWUL, MPROM,
     $                XNE, DDLUL, FDDLUL, CDLUL, LDL, CSHL, WSHL,
     $                W(IPHIS), W(IWNS), W(IWHS), ILFLX, W(ILRYS),
     $                CDSK, WDSK, W(IPHID), W(IWND), W(IWHD), W(ILRYD),
     $                XOBL, IFDB, BKPC, BBC, PBC, PKPC, PJNU, PRXI,
     $                PSIG, PXRD, PZRD, PYRD, W, IW)
C
C     (Give back W allotment)
      call WGIVE     (W, 'HERON')
C     !END
      call BYE ('HERON')
C
      return
      end
