      subroutine GENTIAN
     $(N,NT,XLB1,YBAR,RHOP,SL,AW,EPL,BSL,TAUL,RHWL,NEDL,DROL,ORHO,
     $ IFSL,ILSL,WSML)
C
C     Rudolf Loeser, 1980 May 04
C---- Collects data from Line Intensity Data Blocks, for LILY.
C     !DASH
      save
C     !DASH
      real*8 AW, BSL, DROL, EPL, ORHO, RHOP, RHWL, SL, TAUL, WSML, XLB1,
     $       YBAR, dummy
      integer IFSL, IL, ILSL, IU, IUL, J, KLIN, MMAW, MMBS, MMDRO, MMEP,
     $        MMIFS, MMILS, MMJBR, MMNED, MMORH, MMRHO, MMRHW, MMS,
     $        MMTAU, MMWSM, N, NEDL, NNT, NT
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
      equivalence (LINKDS( 3),KLIN )
C     !EJECT
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(27),MMJBR)
      equivalence (MML(28),MMRHO)
      equivalence (MML(30),MMORH)
      equivalence (MML(26),MMS  )
      equivalence (MML(21),MMEP )
      equivalence (MML(16),MMTAU)
      equivalence (MML(29),MMRHW)
      equivalence (MML(39),MMILS)
      equivalence (MML(40),MMDRO)
      equivalence (MML(37),MMWSM)
      equivalence (MML(41),MMNED)
      equivalence (MML(38),MMIFS)
      equivalence (MML(65),MMAW )
      equivalence (MML(23),MMBS )
C
C---- THULE       as of 1999 Dec 07
      integer     MAXLV,MXTRA
      parameter   (MAXLV=50)
      parameter   (MXTRA=(MAXLV*(MAXLV-1))/2)
C     (Remember to recompile ADAM when changing MAXLV.)
      integer     LMTRA,NUMTRN,NUMBLK,LINNAM,LI1ADR,LI2ADR,LI3ADR
      dimension   LI1ADR(MXTRA),LI2ADR(MXTRA),LI3ADR(MXTRA)
      dimension   LINNAM(MXTRA)
      common      /THULE0/ LMTRA,NUMTRN,NUMBLK
      common      /THULE1/ LINNAM
      common      /THULE2/ LI1ADR
      common      /THULE3/ LI2ADR
      common      /THULE4/ LI3ADR
C
C     Indices and Names of the Line Intensity Data Blocks.
C     LMTRA  -    = MAXLV
C     NUMTRN -    number of transitions (i.e. AIJ .ne. 0), which is
C                 the number of Line Intensity Data Blocks allocated;
C     NUMBLK -    number of Line Intensity Data Blocks in actual use
C                 (i.e. number of radiative and passive transitions);
C     LINNAM -    block name, = 100*IU+IL;
C     LI1ADR -    block address in scratch memory, part 1;
C     LI2ADR -    block address in scratch memory, part 2;
C     LI3ADR -    block address in scratch memory, part 3.
C     .
C     !DASH
      external INTRANS, LIDGET, MOVE1, PET, ZERO1, HI, BYE
C
C               YBAR(N,NT), SL(N,NT), XLB1(Li1len), WSML(NT), ILSL(NT),
      dimension YBAR(N,*),  SL(N,*),  XLB1(*),      WSML(*),  ILSL(*),
C
C               RHOP(N,NT), TAUL(N,NT), NEDL(NT), ORHO(N,NT), IFSL(NT),
     $          RHOP(N,*),  TAUL(N,*),  NEDL(*),  ORHO(N,*),  IFSL(*),
C
C               RHWL(N,NT), DROL(NT), EPL(N,NT), AW(N,NT), BSL(N,NT)
     $          RHWL(N,*),  DROL(*),  EPL(N,*),  AW(N,*),  BSL(N,*)
C     !EJECT
C
      call HI ('GENTIAN')
C     !BEG
C
      NNT = N*NT
C
      call ZERO1       (RHOP, NNT)
      call ZERO1       (YBAR, NNT)
      call ZERO1       (SL  , NNT)
      call ZERO1       (AW  , NNT)
      call ZERO1       (EPL , NNT)
      call ZERO1       (BSL , NNT)
C
      do 100 J = 1,NT
        call PET       (J)
        if(KLIN.eq.1) then
C
          call LIDGET  (XLB1, 1, dummy, 0, dummy, 0, J)
C
          call INTRANS (IU, IL, 'GENTIAN', IUL)
C
          call MOVE1   (XLB1(MMJBR), N, YBAR(1,IUL))
          call MOVE1   (XLB1(MMRHO), N, RHOP(1,IUL))
          call MOVE1   (XLB1(MMORH), N, ORHO(1,IUL))
          call MOVE1   (XLB1(MMS  ), N, SL  (1,IUL))
          call MOVE1   (XLB1(MMAW ), N, AW  (1,IUL))
          call MOVE1   (XLB1(MMEP ), N, EPL (1,IUL))
          call MOVE1   (XLB1(MMBS ), N, BSL (1,IUL))
          call MOVE1   (XLB1(MMTAU), N, TAUL(1,IUL))
          call MOVE1   (XLB1(MMRHW), N, RHWL(1,IUL))
C
          DROL(IUL) = XLB1(MMDRO)
          WSML(IUL) = XLB1(MMWSM)
          NEDL(IUL) = XLB1(MMNED)
          IFSL(IUL) = XLB1(MMIFS)
          ILSL(IUL) = XLB1(MMILS)
C
        end if
  100 continue
C     !END
      call BYE ('GENTIAN')
C
      return
      end
