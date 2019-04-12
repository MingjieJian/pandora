      subroutine BASUTO
     $(X,W,IW,Z,XNE,XMU,CMU,WMU,XSHL,CODSRW,EMSHL,CSHL,WSHL,XDSK,EMDSK,
     $ CDSK,WDSK,VXS,XI,A,WVLUL,YUL,DPUL,DWUL,DDLUL,FDDLUL,CDLUL,GTNUL,
     $ COPUL,BCUL,CSFUL,BUL,BTRUL,DLUL,K,SCNU,XKCNU,IMG,XOBL,ILFLX,
     $ IFDB,MPROM,TNUN,BBC,BKPC,PSIG,PBC,PKPC,PJNU,PRXI,PXRD,PZRD,PYRD,
     $ DMP1)
C
C     Rudolf Loeser, 1983 Mar 03
C---- Sets up ORION Data Blocks.
C     These blocks are records that were initialized by QUIXOTE.
C     In the case of PRD: WN, WH and co-moving PHI will already have
C     been set up as part of the PRD Continuum Jnu Calculation,
C     by BALGIN.
C     See also VULCAN.
C
C     (This is version 2 of BASUTO.)
C     !DASH
      save
C     !DASH
      real*8 A, BBC, BCUL, BKPC, BTRUL, BUL, CDLUL, CDSK, CMU, CODSRW,
     $       COPUL, CSFUL, CSHL, DDLUL, DLLL, DLUL, DPUL, DWUL, EMDSK,
     $       EMSHL, FDDLUL, GTNUL, PBC, PJNU, PKPC, PRXI, PSIG, PXRD,
     $       PYRD, PZRD, R1N, SCNU, TIN, TNUN, TOTIME, TOUT, VXS, W,
     $       WDSK, WMU, WSHL, WVLUL, X, XDSK, XI, XKCNU, XMU, XNE, XOBL,
     $       XSHL, YUL, Z
      integer ICE, IFDB, IL, ILFLX, IMG, INDS, IQSFS, IU, IW, J0, K,
     $        LDL, LG, LL, LNRM, MPROM, MRR, N, NSHL
      logical DMP1, MOVING, SPHERE
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(34),LG )
      equivalence (JZQ(15),MRR)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 23),R1N  )
C     !EJECT
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
C
C---- LINUS       as of 2004 May 12
      integer     LINKDS
      dimension   LINKDS(22)
      common      /LINUS/ LINKDS
C     Line source function calculation control parameters for the
C     current transition as set up by "PET" (and printed by "LINSEED").
C     IU    - index of upper level
C     IL    - index of lower level
C     KLIN  - line "type" code (1: radiative, 2: passive, etc)
C     ICE   - PRD calculation control
C     IPRO  - emergent profiles calculation control
C     METSE - statistical equilibrium calculation method selector
C     METSF - LSF calculation method selector (QR, RT, GR)
C     IBRSW - damping components selector
C     INKSW - input opacity signal
C     LSFT  - LSF solution code (0: full, 1:direct, etc)
C     ILFLX - line flux calculation control
C     LDL   - number of line components
C     LINT  - frequency integration range (half vs. full profile)
C     LSFP  - LSF printout control
C     IFDB  - LSF background control (constant vs. varying)
C     ISBG  - blended line profile plot mode switch
C     KBT   - length of input table XIBLUT
C     KRT   - length of input table XIREDT
C     KST   - length of input table XISYMT
C     KTRN  - length of actual tables XI and DL
C     LOML  - "line-background-continuum-opacity" control
C     ....  - (available)
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
      equivalence (LINKDS( 4),ICE  )
      equivalence (LINKDS(12),LDL  )
C     !EJECT
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ( 31),IQSFS)
C
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
C     !DASH
C     !EJECT
      external BOGDAN, BITTERN, SECOND, ROCKET, MASHED, FIFTY, QUEBEC,
     $         LEVEN, SIOUX, HERON, MOVE1, FOG, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               NRM = 2*N + 5       NP = (N-2)/NTAN + 1
C
C               DLUL(K), FDDLUL(N), XMU(LG), CMU(LG), XNE(N), PBC(N,K),
      dimension DLUL(*), FDDLUL(*), XMU(*),  CMU(*),  XNE(*), PBC(*),
C
C               PKPC(N,K), XSHL(NRM,NSHL), CODSRW(NSHL), EMSHL(N,NSHL),
     $          PKPC(*),   XSHL(*),        CODSRW(*),    EMSHL(*),
C
C               WSHL(N,NSHL), XDSK(N,MRR), CDSK(N,MRR), BBC(N,K), Z(N),
     $          WSHL(*),      XDSK(*),     CDSK(*),     BBC(N,*), Z(*),
C
C               CSHL(N,NSHL), VXS(N), XI(K), EMDSK(N,MRR), WDSK(N,MRR),
     $          CSHL(*),      VXS(*), XI(*), EMDSK(*),     WDSK(*),
C
C               DPUL(N,LDL), PJNU(N,K), DDLUL(LDL), XOBL(Lodlen), A(K),
     $          DPUL(*),     PJNU(*),   DDLUL(*),   XOBL(*),      A(*),
C
C               GTNUL(N), SCNU(N,K), PSIG(N,K), CDLUL(LDL), XKCNU(N,K),
     $          GTNUL(*), SCNU(*),   PSIG(*),   CDLUL(*),   XKCNU(*),
C
C               BCUL(N), DWUL(N), BKPC(N,K), COPUL(N), TNUN(N), IMG(N),
     $          BCUL(*), DWUL(*), BKPC(*),   COPUL(*), TNUN(*), IMG(*),
C
C               BTRUL(N), PRXI(N,K), PXRD(N,K), PYRD(N,K), PZRD(N,K),
     $          BTRUL(*), PRXI(*),   PXRD(*),   PYRD(*),   PZRD(*),
C
C               WMU(LG), BUL(N), CSFUL(N)
     $          WMU(*),  BUL(*), CSFUL(*)
C
      dimension DLLL(1)
C
      call HI ('BASUTO')
C     !BEG
      SPHERE = IQSFS.gt.0
      MOVING = .true.
      if(DMP1) then
        call LEVEN     (IU, IL, N, K, LODLEN, 'BASUTO', 'ORION')
      end if
C
      if(IFDB.gt.0) then
C----   Get detailed background continuum data
        call FIFTY     (X, W, IW, N, K, BKPC, BBC, SCNU, XKCNU)
        call QUEBEC    (XI, K, 'XI', 'BASUTO', J0)
        call MOVE1     (BBC(1,J0), N, BCUL)
      else
C----   Prepare to use line center for the whole line
        call SIOUX     (N, CSFUL, BUL, BTRUL, BCUL)
      end if
C     !EJECT
      if(ICE.ne.0) then
C----   Get PRD-data-arrays
        call FOG       (2, IU, IL, (N*K), PKPC, PBC, PSIG, PJNU, PRXI,
     $                  PXRD, PZRD, PYRD)
      end if
C
C---- Loop over all frequencies
      INDS = 0
      do 100 LL = 1,K
        call SECOND    (TIN)
        DLLL(1) = DLUL(LL)
C----   Get monochromatic optical depth along normal direction
        call BOGDAN    (X, W, IW, WVLUL, DPUL, DWUL, XNE, DDLUL,
     $                  FDDLUL, CDLUL, COPUL, GTNUL, VXS, DLLL, LL,
     $                  TNUN, LNRM, MPROM, IMG)
C----   Set up Data Blocks
        if(SPHERE) then
          call HERON   (X, W, IW, LL, K, XI, A, DLLL, BCUL, GTNUL,
     $                  COPUL, DPUL, DWUL, XNE, DDLUL, FDDLUL, CDLUL,
     $                  LDL, XSHL, CODSRW, Z, R1N, XDSK, EMDSK, CSHL,
     $                  CDSK, WSHL, WDSK, VXS, WVLUL, INDS, ICE,
     $                  ILFLX, IFDB, MPROM, YUL, MOVING, TNUN, LNRM,
     $                  XOBL, IMG, BKPC, BBC, PBC, PKPC, PJNU, PRXI,
     $                  PSIG, PXRD, PZRD, PYRD, DMP1)
          INDS = INDS+(NSHL+MRR)
        else
          call BITTERN (X, W, IW, LL, K, XI, A, DLLL, BCUL, GTNUL,
     $                  COPUL, DPUL, DWUL, XNE, DDLUL, FDDLUL, CDLUL,
     $                  LDL, XMU, CMU, WMU, VXS, WVLUL, INDS, ICE,
     $                  ILFLX, IFDB, MPROM, YUL, MOVING, TNUN, LNRM,
     $                  XOBL, IMG, BKPC, BBC, PBC, PKPC, PJNU, PRXI,
     $                  PSIG, PXRD, PZRD, PYRD, DMP1)
          INDS = INDS+(LG)
        end if
        call SECOND    (TOUT)
        TOTIME = TOUT-TIN
        call ROCKET    (LL, K, TOTIME, 'LSF')
  100 continue
C
      NBOP = INDS
C
      if(DMP1) then
        call MASHED    ('BASUTO')
      end if
C     !END
      call BYE ('BASUTO')
C
      return
      end
