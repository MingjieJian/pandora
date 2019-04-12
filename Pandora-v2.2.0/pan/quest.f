      subroutine QUEST
     $(X,W,IW,XI,A,Z,XNE,VXS,YUL,DPUL,DWUL,DDLUL,FDDLUL,CDLUL,GTNUL,
     $ COPUL,TAUUL,BCUL,CSFUL,BUL,BTRUL,DLUL,K,SCNU,XKCNU,IMG,XPBL,N,
     $ DMP0,DMP1,ILFLX,IFDB,MPROM,MEDUSA)
C
C     Rudolf Loeser, 1980 Feb 08
C---- Sets up a set of DIANA Data Blocks, for transition (IU,IL).
C     (This is version 2 of QUEST.)
C     !DASH
      save
C     !DASH
      real*8 A, BCUL, BTRUL, BUL, CDLUL, COPUL, CSFUL, DDLUL, DLUL,
     $       DPUL, DWUL, FDDLUL, GTNUL, SCNU, TAUUL, VXS, W, X, XI,
     $       XKCNU, XNE, XPBL, YUL, Z
      integer IBC, IFDB, IKILSV, IKISSV, IKPC, ILFLX, IMG, IN, IPBC,
     $        IPJNU, IPKPC, IPRXI, IPSIG, IPXRD, IPYRD, IPZRD, IS,
     $        ITNUSV, IW, IWS, JN, K, MEDUSA, MOX, MPROM, MUX, N, NBAD
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
      external  LANYARD, LADYRAN, VULCAN, FORMOSA, PLUSI, WGIVE, IGIVE,
     $          HI, BYE
      intrinsic min
C
      dimension X(*), W(*), IW(*)
C
C               XPBL(Lpdlen), XI(KM), BCUL(N), DLUL(KM), SCNU(N), Z(N),
      dimension XPBL(*),      XI(*),  BCUL(*), DLUL(*),  SCNU(*), Z(*),
C
C               CDLUL(LDL), COPUL(N), DPUL(N,LDL), DWUL(N), DDLUL(LDL),
     $          CDLUL(*),   COPUL(*), DPUL(*),     DWUL(*), DDLUL(*),
C
C               VXS(N), GTNUL(N), TAUUL(N), XKCNU(N), FDDLUL(N), A(KM),
     $          VXS(*), GTNUL(*), TAUUL(*), XKCNU(*), FDDLUL(*), A(*),
C
C               XNE(N), IMG(N), CSFUL(N), BTRUL(N), BUL(N)
     $          XNE(*), IMG(*), CSFUL(*), BTRUL(*), BUL(*)
C
      dimension IN(11)
      equivalence
     $(IN( 1),IPRXI ),(IN( 2),IPSIG ),(IN( 3),IPBC  ),(IN( 4),IKPC  ),
     $(IN( 5),IBC   ),(IN( 6),IPKPC ),(IN( 7),IPJNU ),(IN( 8),ITNUSV),
     $(IN( 9),IPXRD ),(IN(10),IPYRD ),(IN(11),IPZRD )
C
      dimension JN(2)
      equivalence
     $(JN( 1),IKISSV),(JN( 2),IKILSV)
C     !EJECT
C
      call HI ('QUEST')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call LANYARD   (IN, IS , MOX, 'QUEST')
      call LADYRAN   (JN, IWS, MUX, 'QUEST')
C
      call VULCAN    (X, W, IW, XI, A, Z, XNE, VXS, YUL, DPUL, DWUL,
     $                DDLUL, FDDLUL, CDLUL, GTNUL, COPUL, BCUL, CSFUL,
     $                BUL, BTRUL, DLUL, K, N, ILFLX, IFDB, MPROM,
     $                SCNU, XKCNU, XPBL, W(IKPC), W(IBC), W(IPBC),
     $                W(IPKPC), W(IPJNU), W(IPRXI ), W(IPSIG),
     $                W(IPXRD), W(IPZRD), W(IPYRD), IMG, W(ITNUSV),
     $                IW(IKISSV), IW(IKILSV), DMP1)
C
      if(DMP0) then
        call FORMOSA (XPBL, N, ILFLX)
      end if
C
      call PLUSI     (KEROP, 1, NBOP, NBAD)
      MEDUSA = min((NBOP-NBAD),1)
C
C     (Give back W & IW allotments)
      call WGIVE     (W , 'QUEST')
      call IGIVE     (IW, 'QUEST')
C     !END
      call BYE ('QUEST')
C
      return
      end
