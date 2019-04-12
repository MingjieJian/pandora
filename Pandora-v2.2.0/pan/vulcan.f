      subroutine VULCAN
     $(X,W,IW,XI,A,Z,XNE,VXS,YUL,DPUL,DWUL,DDLUL,FDDLUL,CDLUL,GTNUL,
     $ COPUL,BCUL,CSFUL,BUL,BTRUL,DL,K,N,ILFLX,IFDB,MPROM,SCNU,XKCNU,
     $ XPBL,BKPC,BBC,PBC,PKPC,PJNU,PRXI,PSIG,PXRD,PZRD,PYRD,IMG,
     $ TNUSAV,KISSAV,KILSAV,DMP1)
C
C     Rudolf Loeser, 1980 Feb 07
C---- Sets up Diana Data Blocks.
C
C     These Blocks are records that were initialized by QUIXOTE.
C     In the case of PRD: WN and WH will already have been updated
C     as part of the PRD Continuum Jnu Calculation, by ELKIN.
C     See also BASUTO.
C
C     (This is version 2 of VULCAN.)
C     !DASH
      save
C     !DASH
      real*8 A, BBC, BCUL, BKPC, BTRUL, BUL, CDLUL, COPUL, CSFUL, DDLUL,
     $       DL, DPUL, DWUL, FDDLUL, GTNUL, PBC, PJNU, PKPC, PRXI, PSIG,
     $       PXRD, PYRD, PZRD, SCNU, TNUSAV, VXS, W, X, XI, XKCNU, XNE,
     $       XPBL, YUL, Z
      integer ICE, IFDB, IL, ILFLX, IMG, ISR, IU, IW, J0, K, KILSAV,
     $        KISSAV, KNT, KODE, KTNU, LL, LPNAM, MIK, MPROM, N
      logical DMP1
C     !COM
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
C     !EJECT
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
C---- PERBLOC     as of 2005 Jan 21
      integer     LPDLEN,LPD
      dimension   LPD(15)
      common      /PERBLOC/ LPDLEN,LPD
C     This is the "DIANA" Data Block index, for the calculation of
C     line source functions in a static atmosphere.
      equivalence (LPD( 1),LPNAM)
C     !DASH
C     !EJECT
      external  AUDLEY, LEYTE, BERYL, LEVEN, FOG, CULVAN, FIFTY, MALET,
     $          BENWIG, BOHOL, NANATU, UNTANA, SIOUX, MASHED, QUEBEC,
     $          MOVE1, HI, BYE
      intrinsic max
C
      dimension X(*), W(*), IW(*)
C
C               A(K), DL(K), XI(K), VXS(N), XNE(N), GTNUL(N), PBC(N,K),
      dimension A(*), DL(*), XI(*), VXS(*), XNE(*), GTNUL(*), PBC(*),
C
C               BKPC(N,K), DWUL(N), DDLUL(LDL), XPBL(Lpdlen), BBC(N,K),
     $          BKPC(*),   DWUL(*), DDLUL(*),   XPBL(*),      BBC(N,*),
C
C               PKPC(N,K), SCNU(N), BCUL(N), XKCNU(N), Z(N), PRXI(N,K),
     $          PKPC(*),   SCNU(*), BCUL(*), XKCNU(*), Z(*), PRXI(*),
C
C               COPUL(N), PSIG(N,K), DPUL(N,LDL), PJNU(N,K), FDDLUL(N),
     $          COPUL(*), PSIG(*),   DPUL(*),     PJNU(*),   FDDLUL(*),
C
C               TNUSAV(N,KM), PYRD(N,K), CDLUL(LDL), IMG(N), PZRD(N,K),
     $          TNUSAV(*),    PYRD(*),   CDLUL(*),   IMG(*), PZRD(*),
C
C               PXRD(N,K), KISSAV(KM), KILSAV(KM), CSFUL(N), BTRUL(N),
     $          PXRD(*),   KISSAV(*),  KILSAV(*),  CSFUL(*), BTRUL(*),
C
C               BUL(N)
     $          BUL(*)
C
      call HI ('VULCAN')
C     !BEG
      if(DMP1) then
        call LEVEN    (IU, IL, N, K, LPDLEN, 'VULCAN', 'DIANA')
      end if
C
      if(IFDB.gt.0) then
C----   Get detailed background continuum data
        call FIFTY    (X, W, IW, N, K, BKPC, BBC, SCNU, XKCNU)
        call QUEBEC   (XI, K, 'XI', 'VULCAN', J0)
        call MOVE1    (BBC(1,J0), N, BCUL)
      else
C----   Prepare to use line center for the whole line
        call SIOUX    (N, CSFUL, BUL, BTRUL, BCUL)
      end if
C
C---- Get TNU-analysis switch
      call NANATU     (KTNU, KNT)
C     !EJECT
      if(ICE.ne.0) then
C----   Get PRD-data-arrays
        call FOG      (2, IU, IL, (N*K), PKPC, PBC, PSIG, PJNU, PRXI,
     $                 PXRD, PZRD, PYRD)
      end if
C
C---- Loop over all integration frequency points
      do 100 LL = 1,K
C----   Get data block record, for updating
        call LEYTE    (XPBL, LPDLEN, INDOP(LL))
        call AUDLEY   (XPBL(LPNAM), LL, 'DIANA')
C----   Set up this block
        call CULVAN   (X, W, IW, XPBL, LL, ICE, XI, A, Z, XNE, VXS,
     $                 YUL, DPUL, DWUL, DDLUL, FDDLUL, CDLUL, GTNUL,
     $                 COPUL, BCUL, DL, K, N, ILFLX, IFDB, MPROM, BKPC,
     $                 BBC, PBC, PKPC, PJNU, PRXI, PSIG, PXRD, PZRD,
     $                 PYRD, IMG, MIK, ISR, KODE, KTNU, TNUSAV, KISSAV,
     $                 KILSAV, KNT, DMP1)
C----   Process error flags
        call BERYL    (MIK, ISR, KODE, LL)
C----   Save or discard this block
        if(KODE.eq.0) then
          call BOHOL  (XPBL, LPDLEN, INDOP(LL))
        else
          call BENWIG (LL, KODE)
        end if
C
        if(DMP1) then
          call MALET  (LL, NBOP, MIK, ISR, KODE)
        end if
  100 continue
      if(DMP1) then
        call MASHED   ('VULCAN')
      end if
C
      NBOP = K
C
      if(KNT.gt.0) then
C----   Print TNU-analysis
        call UNTANA   (N, K, XI, DL, TNUSAV, KISSAV, KILSAV)
      end if
C     !END
      call BYE ('VULCAN')
C
      return
      end
