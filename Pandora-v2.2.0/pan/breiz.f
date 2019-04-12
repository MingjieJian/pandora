      subroutine BREIZ
     $(LL,K,XI,A,DL,N,LG,IND,ICE,TNUN,LNRM,BCUL,COPUL,DPUL,DWUL,MPROM,
     $ XNE,DDLUL,FDDLUL,CDLUL,LDL,CMU,WMU,PHIA,WNA,RAYL,WHA,ILFLX,XOBL,
     $ IFDB,BKPC,BBC,PBC,PKPC,PJNU,PRXI,PSIG,PXRD,PZRD,PYRD,VXX,W,IW)
C
C     Rudolf Loeser, 1983 Mar 02
C---- Sets up ORION Data Blocks, for GORMUND.
C     !DASH
      save
C     !DASH
      real*8 A, BBC, BCUL, BKPC, CDLUL, CMU, COPUL, CORE, DDLUL, DL,
     $       DPUL, DWUL, FDDLUL, PBC, PHIA, PJNU, PKPC, PRXI, PSIG,
     $       PXRD, PYRD, PZRD, RAYL, TNUN, VXX, W, WHA, WMU, WNA, XI,
     $       XNE, XOBL, ZERO
      integer ICE, IFDB, ILFLX, IND, IW, K, K1, KODE, LDL, LG, LL, LNRM,
     $        LOALL, LOBC, LOCWT, LOJNU, LOKPC, LONAM, LOPHI, LOPHIW,
     $        LORXI, LOSIG, LOT1, LOT2, LOT3, LOTNU, LOWH, LOWN, LOWWT,
     $        LOXLL, MM, MPROM, N, NN
C     !COM
C---- ORIBLOC     as of 2005 Jan 21
      integer     LODLEN,LOD
      dimension   LOD(17)
      common      /ORIBLOC/ LODLEN,LOD
C     This is the "ORION" Data Block index, for the calculation
C     of line source functions in an expanding atmosphere.
C     .
      equivalence
     $(LOD( 1),LONAM ),(LOD( 2),LOPHIW),(LOD( 3),LOKPC ),
     $(LOD( 4),LOPHI ),(LOD( 5),LOCWT ),(LOD( 6),LOWN  ),
     $(LOD( 7),LOBC  ),(LOD( 8),LOTNU ),(LOD( 9),LOJNU ),
     $(LOD(10),LOT1  ),(LOD(11),LOSIG ),(LOD(12),LOALL ),
     $(LOD(13),LOXLL ),(LOD(14),LOWH  ),(LOD(15),LOWWT ),
     $(LOD(16),LOT2  ),(LOD(17),LOT3  )
C     .
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
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external YORRA, SHINE, SET1, ZERO1, BOHOL, MOVE1, LOTUKO, AUDLEY,
     $         YMUIR, LEYTE, HI, BYE
C
      dimension W(*), IW(*)
C
C               DWUL(N), A(K), PHIA(N,N,LG), WNA(N,N,LG), XOBL(Lodlen),
      dimension DWUL(*), A(*), PHIA(N,N,*),  WNA(N,N,*),  XOBL(*),
C
C               CMU(LG), WHA(N,N,LG), WMU(LG), XNE(N), XI(K), COPUL(N),
     $          CMU(*),  WHA(N,N,*),  WMU(*),  XNE(*), XI(*), COPUL(*),
C
C               TNUN(N), BCUL(N), BBC(N,K), DPUL(N,LDL), VXX(N), DL(1),
     $          TNUN(*), BCUL(*), BBC(*),   DPUL(*),     VXX(*), DL(*),
C
C               DDLUL(LDL), PSIG(N,K), PBC(N,K), CDLUL(LDL), FDDLUL(N),
     $          DDLUL(*),   PSIG(*),   PBC(*),   CDLUL(*),   FDDLUL(*),
C
C               PKPC(N,K), PJNU(N,K), PXRD(N,K), BKPC(N,K), PRXI(N,K),
     $          PKPC(*),   PJNU(*),   PXRD(*),   BKPC(*),   PRXI(*),
C
C               PYRD(N,K), RAYL(LG), PZRD(N,K)
     $          PYRD(*),   RAYL(*),  PZRD(*)
C
      data      CORE,K1 /0.D0, 1/
C     !EJECT
C
      call HI ('BREIZ')
C     !BEG
      NN = N**2
      do 100 MM = 1,LG
        IND = IND+1
C----   Get record containing data block, for updating
        call LEYTE     (XOBL, LODLEN, INDOP(IND))
        call AUDLEY    (XOBL(LONAM), IND, 'ORION')
        XOBL(LOXLL) = XI(LL)
        XOBL(LOALL) = A(LL)
C----   Set up TNU, XKPC, and BC, and associated PRD data
        call MOVE1     (TNUN, N, XOBL(LOTNU))
        call SHINE     (LL, K, N, ICE, IFDB, BCUL, COPUL, XOBL(LOBC),
     $                  XOBL(LOKPC), XOBL(LOJNU), XOBL(LOSIG),
     $                  XOBL(LOT1), XOBL(LOT2), XOBL(LOT3), BKPC, BBC,
     $                  PBC, PKPC, PJNU, PRXI, PSIG, PXRD, PZRD, PYRD)
C----   Set up angle integration weights
        call SET1      (XOBL(LOCWT), N, CMU(MM))
        call SET1      (XOBL(LOWWT), N, WMU(MM))
C----   Set up symmetric profile
        call ZERO1     (VXX, N)
        call YMUIR     (W, IW, CORE, DL, K1, DPUL, DWUL, XNE, VXX, N,
     $                  DDLUL, FDDLUL, CDLUL, LDL, MPROM, XOBL(LOPHI))
        if(ICE.eq.0) then
C----     Set up comoving profile, weight matrices
          call MOVE1   (PHIA(1,1,MM), NN, XOBL(LOPHIW))
          call MOVE1   (WNA (1,1,MM), NN, XOBL(LOWN)  )
          if(ILFLX.gt.0) then
            call MOVE1 (WHA (1,1,MM), NN, XOBL(LOWH)  )
          end if
        end if
C----   Process error flags
        call LOTUKO    (LNRM, RAYL(MM), KODE, IND)
C----   Save or discard this block
        if(KODE.eq.0) then
          call BOHOL   (XOBL, LODLEN, INDOP(IND))
        else
          call YORRA   (IND, KODE, XOBL(LONAM))
        end if
  100 continue
C     !END
      call BYE ('BREIZ')
C
      return
      end
