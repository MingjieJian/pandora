      subroutine CYMRY
     $(LL,K,XI,A,DL,N,NSHL,NRPMX,IND,ICE,TNUN,LNRM,BCUL,COPUL,DPUL,
     $ DWUL,MPROM,XNE,DDLUL,FDDLUL,CDLUL,LDL,CSHL,WSHL,PHISHL,WNSHL,
     $ RYSHLL,WHSHL,ILFLX,XOBL,IFDB,BKPC,BBC,PBC,PKPC,PJNU,PRXI,PSIG,
     $ PXRD,PZRD,PYRD,VXX,W,IW)
C
C     Rudolf Loeser, 1983 Mar 02
C---- Sets up ORION Data Blocks, for shell rays, for CALEDON.
C     !DASH
      save
C     !DASH
      real*8 A, BBC, BCUL, BKPC, CDLUL, COPUL, CORE, CSHL, DDLUL, DL,
     $       DPUL, DWUL, FDDLUL, PBC, PHISHL, PJNU, PKPC, PRXI, PSIG,
     $       PXRD, PYRD, PZRD, RYSHLL, TNUN, VXX, W, WHSHL, WNSHL, WSHL,
     $       XI, XNE, XOBL
      integer I, ICE, IFDB, ILFLX, IND, IW, K, K1, KODE, LDL, LL, LNRM,
     $        LOALL, LOBC, LOCWT, LOJNU, LOKPC, LONAM, LOPHI, LOPHIW,
     $        LORXI, LOSIG, LOT1, LOT2, LOT3, LOTNU, LOWH, LOWN, LOWWT,
     $        LOXLL, MM, MPROM, N, NN, NRPMX, NSHL
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
C     !DASH
C     !EJECT
      external TATAR, LEYTE, SHINE, ZERO1, YMUIR, MOVE1, LOTUKO, BOHOL,
     $         YORRA, AUDLEY, HI, BYE
C
      dimension W(*), IW(*)
C
C               VXX(N), A(K), CSHL(N,NSHL), RYSHLL(NSHL), WSHL(N,NSHL),
      dimension VXX(*), A(*), CSHL(N,*),    RYSHLL(*),    WSHL(N,*),
C
C               PHISHL(N,N,NSHL), WNSHL(N,N,NSHL), TNUN(N), CDLUL(LDL),
     $          PHISHL(N,N,*),    WNSHL(N,N,*),    TNUN(*), CDLUL(*),
C
C               XOBL(Lodlen), BCUL(N), COPUL(N), DDLUL(LDL), PJNU(N,K),
     $          XOBL(*),      BCUL(*), COPUL(*), DDLUL(*),   PJNU(*),
C
C               BKPC(N,K), BBC(N,K), FDDLUL(N), DL(1), WHSHL(N,N,NSHL),
     $          BKPC(*),   BBC(*),   FDDLUL(*), DL(*), WHSHL(N,N,*),
C
C               DWUL(N,K), DPUL(N,LDL), PSIG(N,K), PRXI(N,K), PBC(N,K),
     $          DWUL(*),   DPUL(*),     PSIG(*),   PRXI(*),   PBC(*),
C
C               PKPC(N,K), XNE(N), PXRD(N,K), PYRD(N,K), PZRD(N,K),
     $          PKPC(*),   XNE(*), PXRD(*),   PYRD(*),   PZRD(*),
C
C               XI(K)
     $          XI(*)
C
      data CORE,K1 /0.D0, 1/
C     !EJECT
C
      call HI ('CYMRY')
C     !BEG
      NN = N**2
      I  = 0
C
      do 100 MM = 1,NSHL
        IND = IND+1
        call TATAR     (I)
C----   Get record containing data block, for updating
        call LEYTE     (XOBL, LODLEN, INDOP(IND))
        call AUDLEY    (XOBL(LONAM), IND, 'ORION')
        XOBL(LOXLL) = XI(LL)
        XOBL(LOALL) = A(LL)
C----   Set up TNU, XKPC and BC, and associated PRD data
        call MOVE1     (TNUN, N, XOBL(LOTNU))
        call SHINE     (LL, K, N, ICE, IFDB, BCUL, COPUL, XOBL(LOBC),
     $                  XOBL(LOKPC), XOBL(LOJNU), XOBL(LOSIG),
     $                  XOBL(LOT1), XOBL(LOT2), XOBL(LOT3), BKPC, BBC,
     $                  PBC, PKPC, PJNU, PRXI, PSIG, PXRD, PZRD, PYRD)
C----   Set up angle integration weights
        call ZERO1     (XOBL(LOCWT), N)
        call MOVE1     (CSHL(1,MM), I, XOBL(LOCWT))
        call ZERO1     (XOBL(LOWWT), N)
        call MOVE1     (WSHL(1,MM), I, XOBL(LOWWT))
C----   Set up symmetric profile
        call ZERO1     (VXX, N)
        call YMUIR     (W, IW, CORE, DL, K1, DPUL, DWUL, XNE, VXX, N,
     $                  DDLUL, FDDLUL, CDLUL, LDL, MPROM, XOBL(LOPHI))
        if(ICE.eq.0) then
C----     Set up comoving profile, weight matrices
          call MOVE1   (PHISHL(1,1,MM), NN, XOBL(LOPHIW))
          call MOVE1   (WNSHL(1,1,MM),  NN, XOBL(LOWN)  )
          if(ILFLX.gt.0) then
            call MOVE1 (WHSHL(1,1,MM),  NN, XOBL(LOWH)  )
          end if
        end if
C----   Process error flags
        call LOTUKO    (LNRM, RYSHLL(MM), KODE, IND)
C----   Save or discard this block
        if(KODE.eq.0) then
          call BOHOL   (XOBL, LODLEN, INDOP(IND))
        else
          call YORRA   (IND, KODE, XOBL(LONAM))
        end if
  100 continue
C     !END
      call BYE ('CYMRY')
C
      return
      end
