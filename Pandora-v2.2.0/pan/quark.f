      subroutine QUARK
     $(X,W,IW,Z,XNE,XMU,CMU,WMU,XSHL,CODSRW,EMSHL,CSHL,WSHL,XDSK,EMDSK,
     $ CDSK,WDSK,VXS,XI,A,WVLUL,YUL,DPUL,DWUL,DDLUL,FDDLUL,CDLUL,GTNUL,
     $ COPUL,BCUL,CSFUL,BUL,BTRUL,DLUL,K,SCNU,XKCNU,IMG,XOBL,ILFLX,
     $ IFDB,MPROM,MEDUSA,DMP0,DMP1)
C
C     Rudolf Loeser, 1998 Aug 05
C---- Sets up a set of ORION Data Blocks, for transition (IU,IL).
C     !DASH
      save
C     !DASH
      real*8 A, BCUL, BTRUL, BUL, CDLUL, CDSK, CMU, CODSRW, COPUL,
     $       CSFUL, CSHL, DDLUL, DLUL, DPUL, DWUL, EMDSK, EMSHL, FDDLUL,
     $       GTNUL, SCNU, VXS, W, WDSK, WMU, WSHL, WVLUL, X, XDSK, XI,
     $       XKCNU, XMU, XNE, XOBL, XSHL, YUL, Z
      integer IBBC, IBKPC, IFDB, ILFLX, IMG, IN, IPBC, IPJNU, IPKPC,
     $        IPRXI, IPSIG, IPXRD, IPYRD, IPZRD, IS, ITNUN, IW, K,
     $        MEDUSA, MOX, MPROM, NBAD
      logical DMP0, DMP1
C     !COM
C---- URANUS      as of 2005 Dec 12
      integer     LEMUR
      parameter   (LEMUR=30000)
      integer     LIMPID,IUOP,ILOP,NBOP,MBOP,KEROP,INDOP
      real*8      OPNAM
      dimension   OPNAM(LEMUR),KEROP(LEMUR),INDOP(LEMUR)
      common      /URANUS1/ LIMPID,IUOP,ILOP,NBOP,MBOP
      common      /URANUS2/ KEROP
      common      /URANUS3/ INDOP
      common      /URANUS4/ OPNAM
C     Record index for Diana/Orion Data Blocks.
C
C     KEROP .eq. 0: acceptable data block, use it;
C     KEROP .gt. 0: defective data block, do not use it.
C     (The various values of KEROP signify different error conditions).
C     .
C     !DASH
C     !EJECT
      external  MALUTI, DINANG, BASUTO, PLUSI, WGIVE, HI, BYE
      intrinsic min
C
      dimension X(*), W(*), IW(*)
C
C               NRM = 2*N + 5       NP = (N-2)/NTAN + 1
C
C               DLUL(KM), GTNUL(N), XMU(LG), CMU(LG), XNE(N), COPUL(N),
      dimension DLUL(*),  GTNUL(*), XMU(*),  CMU(*),  XNE(*), COPUL(*),
C
C               XKCNU(N,KM), VXS(N), BCUL(N), EMDSK(N,MRR), CDLUL(LDL),
     $          XKCNU(*),    VXS(*), BCUL(*), EMDSK(*),     CDLUL(*),
C
C               SCNU(N,KM), XSHL(NRM,NSHL), CODSRW(NSHL), XOBL(Lodlen),
     $          SCNU(*),   XSHL(*),        CODSRW(*),     XOBL(*),
C
C               CSHL(N,NSHL), WSHL(N,NSHL), XDSK(N,MRR), XI(KM), A(KM),
     $          CSHL(*),      WSHL(*),      XDSK(*),     XI(*),  A(*),
C
C               DPUL(N,LDL), DWUL(N), DDLUL(LDL), CDSK(N,MRR), WMU(LG),
     $          DPUL(*),     DWUL(*), DDLUL(*),   CDSK(*),     WMU(*),
C
C               BTRUL(N), WDSK(N,MRR), CSFUL(N), EMSHL(N,NSHL), BUL(N),
     $          BTRUL(*), WDSK(*),     CSFUL(*), EMSHL(*),      BUL(*),
C
C               IMG(N), FDDLUL(N), Z(N)
     $          IMG(*), FDDLUL(*), Z(*)
C
      dimension IN(11)
      equivalence
     $(IN( 1),ITNUN ),(IN( 2),IBBC  ),(IN( 3),IBKPC ),(IN( 4),IPSIG ),
     $(IN( 5),IPBC  ),(IN( 6),IPKPC ),(IN( 7),IPJNU ),(IN( 8),IPRXI ),
     $(IN( 9),IPXRD ),(IN(10),IPYRD ),(IN(11),IPZRD )
C     !EJECT
C
      call HI ('QUARK')
C     !BEG
C     (Get, and allocate, W allotment)
      call MALUTI   (IN, IS, MOX, 'QUARK')
C
      call BASUTO   (X, W, IW, Z, XNE, XMU, CMU, WMU, XSHL, CODSRW,
     $               EMSHL, CSHL, WSHL, XDSK, EMDSK, CDSK, WDSK, VXS,
     $               XI, A, WVLUL, YUL, DPUL, DWUL, DDLUL, FDDLUL,
     $               CDLUL, GTNUL, COPUL, BCUL, CSFUL, BUL, BTRUL,
     $               DLUL, K, SCNU, XKCNU, IMG, XOBL, ILFLX, IFDB,
     $               MPROM, W(ITNUN), W(IBBC), W(IBKPC), W(IPSIG),
     $               W(IPBC), W(IPKPC), W(IPJNU), W(IPRXI), W(IPXRD),
     $               W(IPZRD), W(IPYRD), DMP1)
C
      if(DMP0) then
        call DINANG (XOBL, ILFLX)
      end if
C
      call PLUSI    (KEROP, 1, NBOP, NBAD)
      MEDUSA = min((NBOP-NBAD),1)
C
C     (Give back W allotment)
      call WGIVE    (W, 'QUARK')
C     !END
      call BYE ('QUARK')
C
      return
      end
