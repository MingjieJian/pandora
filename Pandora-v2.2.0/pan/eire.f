      subroutine EIRE
     $(LL,K,XI,A,DL,N,MRR,IND,ICE,TNUN,LNRM,BCUL,COPUL,DPUL,DWUL,MPROM,
     $ XNE,DDLUL,FDDLUL,CDLUL,LDL,CDSK,WDSK,PHIDSK,WNDSK,RYDSKL,WHDSK,
     $ ILFLX,XOBL,IFDB,BKPC,BBC,PBC,PKPC,PJNU,PRXI,PSIG,PXRD,PZRD,PYRD,
     $ VXX,W,IW)
C
C     Rudolf Loeser, 1983 Mar 02
C---- Sets up ORION Data Blocks, for disk rays, for CALEDON.
C     !DASH
      save
C     !DASH
      real*8 A, BBC, BCUL, BKPC, CDLUL, CDSK, COPUL, CORE, DDLUL, DL,
     $       DPUL, DWUL, FDDLUL, PBC, PHIDSK, PJNU, PKPC, PRXI, PSIG,
     $       PXRD, PYRD, PZRD, RYDSKL, TNUN, VXX, W, WDSK, WHDSK, WNDSK,
     $       XI, XNE, XOBL
      integer ICE, IFDB, ILFLX, IND, IW, K, K1, KODE, LDL, LL, LNRM,
     $        LOALL, LOBC, LOCWT, LOJNU, LOKPC, LONAM, LOPHI, LOPHIW,
     $        LORXI, LOSIG, LOT1, LOT2, LOT3, LOTNU, LOWH, LOWN, LOWWT,
     $        LOXLL, MM, MPROM, MRR, N, NN
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
      external SHINE, ZERO1, YMUIR, MOVE1, LOTUKO, LEYTE, BOHOL, YORRA,
     $         AUDLEY, HI, BYE
C
      dimension W(*), IW(*)
C
C               A(K), CDSK(N,MRR), RYDSKL(MRR), WDSK(N,MRR), BKPC(N,K),
      dimension A(*), CDSK(N,*),   RYDSKL(*),   WDSK(N,*),   BKPC(*),
C
C               PHIDSK(N,N,MRR), WNDSK(N,N,MRR), WHDSK(N,N,MRR), XI(K),
     $          PHIDSK(N,N,*),   WNDSK(N,N,*),   WHDSK(N,N,*),   XI(*),
C
C               DL(1), TNUN(N), BCUL(N), COPUL(N), DPUL(N,LDL), VXX(N),
     $          DL(*), TNUN(*), BCUL(*), COPUL(*), DPUL(*),     VXX(*),
C
C               PBC(N,K), DDLUL(LDL), CDLUL(LDL), PSIG(N,K), PRXI(N,K),
     $          PBC(*),   DDLUL(*),   CDLUL(*),   PSIG(*),   PRXI(*),
C
C               XOBL(Lodlen), FDDLUL(N), DWUL(N), PKPC(N,K), PJNU(N,K),
     $          XOBL(*),      FDDLUL(*), DWUL(*), PKPC(*),   PJNU(*),
C
C               XNE(N), BBC(N,K), PXRD(N,K), PYRD(N,K), PZRD(N,K)
     $          XNE(*), BBC(*),   PXRD(*),   PYRD(*),   PZRD(*)
C
      data CORE,K1 /0.D0, 1/
C     !EJECT
C
      call HI ('EIRE')
C     !BEG
      NN = N**2
C
      do 100 MM = 1,MRR
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
        call MOVE1     (CDSK(1,MM), N, XOBL(LOCWT))
        call MOVE1     (WDSK(1,MM), N, XOBL(LOWWT))
C----   Set up symmetric profile
        call ZERO1     (VXX, N)
        call YMUIR     (W, IW, CORE, DL, K1, DPUL, DWUL, XNE, VXX, N,
     $                  DDLUL, FDDLUL, CDLUL, LDL, MPROM, XOBL(LOPHI))
        if(ICE.eq.0) then
C----     Set up comoving profile, weight matrices
          call MOVE1   (PHIDSK(1,1,MM), NN, XOBL(LOPHIW))
          call MOVE1   (WNDSK(1,1,MM),  NN, XOBL(LOWN)  )
          if(ILFLX.gt.0) then
            call MOVE1 (WHDSK(1,1,MM),  NN, XOBL(LOWH)  )
          end if
        end if
C----   Process error flags
        call LOTUKO    (LNRM, RYDSKL(MM), KODE, IND)
C----   Save or discard this block
        if(KODE.eq.0) then
          call BOHOL   (XOBL, LODLEN, INDOP(IND))
        else
          call YORRA   (IND, KODE, XOBL(LONAM))
        end if
  100 continue
C     !END
      call BYE ('EIRE')
C
      return
      end
