      subroutine ELIDYR
     $(X,XLB1,XLB2,XLB3,Y,CRD,CVW,CSK,CRS,COP,RHW,CDL,DDL,DWN,WSM,DRO,
     $ XC,XP,XR,GMA,PGL,XIB,XIR,XIS,XLAM,STNE,DPM,IFS,ILS,NED,ISB1,
     $ ISB2,IST,KST)
C
C     Rudolf Loeser, 1999 Nov 17
C---- Initializes and writes the Line Intensity Data Blocks.
C
C==== NOTE that NUMTRN = NT, according to subroutine PIPIT.
C
C     (This is version 2 of ELIDYR.)
C     !DASH
      save
C     !DASH
      real*8 CDL, COP, CRD, CRS, CSK, CVW, DDL, DPM, DRO, DWN, GMA, PGL,
     $       RHW, STNE, WSM, X, XC, XIB, XIR, XIS, XLAM, XLB1, XLB2,
     $       XLB3, XP, XR, Y
      integer I, IFS, IL, ILS, ISB1, ISB2, IST, IU, IUL, JJAW, JJQHI,
     $        JJRHO, JJYBR, KBTMX, KRTMX, KST, KSTMX, LDLMX, MMCDL,
     $        MMCOP, MMCRD, MMCRS, MMCSK, MMCVW, MMDDL, MMDPM, MMDRO,
     $        MMDWN, MMGMA, MMIFS, MMILS, MMIXB, MMIXR, MMIXS, MMLAM,
     $        MMLRI, MMNAM, MMNED, MMPGL, MMRHW, MMSB1, MMSB2, MMSTE,
     $        MMSTI, MMSTK, MMWSM, MMXC, MMXP, MMXR, MMY, MMYAM, MMZAM,
     $        N, NED, NOION, NT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 5),NT )
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
      equivalence (KZQ( 94),NOION)
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
      equivalence (LEST(33),LDLMX)
      equivalence (LEST(60),KBTMX)
      equivalence (LEST(61),KRTMX)
      equivalence (LEST(62),KSTMX)
C     !EJECT
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 45),JJRHO)
      equivalence (IZOQ( 13),JJYBR)
      equivalence (IZOQ(245),JJQHI)
      equivalence (IZOQ(242),JJAW )
C
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML( 7),MMCRD)
      equivalence (MML( 8),MMCVW)
      equivalence (MML( 9),MMCSK)
      equivalence (MML(10),MMCRS)
      equivalence (MML( 5),MMY  )
      equivalence (MMT( 1),MMZAM)
      equivalence (MML(17),MMCOP)
      equivalence (MML(29),MMRHW)
      equivalence (MML(38),MMIFS)
      equivalence (MML(39),MMILS)
      equivalence (MML(40),MMDRO)
      equivalence (MML(41),MMNED)
      equivalence (MML(43),MMXP )
      equivalence (MML(42),MMXC )
      equivalence (MML(44),MMGMA)
      equivalence (MMP( 1),MMYAM)
      equivalence (MML(34),MMDDL)
      equivalence (MML(55),MMDWN)
      equivalence (MML(49),MMSB1)
      equivalence (MML(50),MMSB2)
      equivalence (MML(53),MMPGL)
      equivalence (MML(56),MMIXB)
      equivalence (MML(57),MMIXR)
      equivalence (MML(58),MMIXS)
      equivalence (MML(60),MMSTI)
      equivalence (MML(62),MMSTK)
      equivalence (MML(37),MMWSM)
      equivalence (MML(31),MMCDL)
      equivalence (MML( 1),MMNAM)
      equivalence (MML( 2),MMLAM)
      equivalence (MML(61),MMSTE)
      equivalence (MML( 6),MMDPM)
      equivalence (MML(64),MMLRI)
      equivalence (MML(63),MMXR )
C     !EJECT
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
C     !DASH
C     !EJECT
      external INTRANS, MOVE1, ZERO1, MALLARD, LIDMAK, DRECK, KORINA,
     $         PET, HI, BYE
C
      dimension X(*)
C
C               CRD(LDLMX,NT), CVW(LDLMX,NT), CSK(LDLMX,NT), CRS(NT),
      dimension CRD(LDLMX,*),  CVW(LDLMX,*),  CSK(LDLMX,*),  CRS(*),
C
C               CDL(LDLMX,NT), DDL(LDLMX,NT), DWN(LDLMX,NT), WSM(NT),
     $          CDL(LDLMX,*),  DDL(LDLMX,*),  DWN(LDLMX,*),  WSM(*),
C
C               XIB(KBTMX,NT), XIR(KRTMX,NT), XIS(KSTMX,NT), DRO(NT),
     $          XIB(KBTMX,*),  XIR(KRTMX,*),  XIS(KSTMX,*),  DRO(*),
C
C               XLB1(Li1len), RHW(N,NT), COP(N,NT), IST(NT), KST(NT),
     $          XLB1(*),      RHW(N,*),  COP(N,*),  IST(*),  KST(*),
C
C               XC(NT), STNE(NT), PGL(NT), IFS(NT), ILS(NT), NED(NT),
     $          XC(*),  STNE(*),  PGL(*),  IFS(*),  ILS(*),  NED(*),
C
C               ISB1(NT), ISB2(NT), DPM(NT), XP(NT), Y(NT), XLAM(NT),
     $          ISB1(*),  ISB2(*),  DPM(*),  XP(*),  Y(*),  XLAM(*),
C
C               GMA(NT), XLB2(Li2len), XLB3(Li3len), XR(NT)
     $          GMA(*),  XLB2(*),      XLB3(*),      XR(*)
C
      call HI ('ELIDYR')
C     !BEG
      if(NOION.le.0) then
        call ZERO1     (XLB2, LI2LEN)
        call ZERO1     (XLB3, LI3LEN)
C     !EJECT
        do 100 I = 1,NUMTRN
          call PET     (I)
          call INTRANS (IU, IL, 'ELIDYR', IUL)
          call ZERO1   (XLB1, LI1LEN)
C
          XLB1(MMNAM) = LINNAM(I)
          XLB2(MMYAM) = LINNAM(I)
          XLB3(MMZAM) = LINNAM(I)
C
          XLB1(MMLAM) = XLAM(IUL)
          XLB1(MMY  ) = Y(IUL)
          XLB1(MMCRS) = CRS(IUL)
          XLB1(MMDPM) = DPM(IUL)
          XLB1(MMWSM) = WSM(IUL)
          XLB1(MMDRO) = DRO(IUL)
          XLB1(MMXC ) = XC(IUL)
          XLB1(MMXP ) = XP(IUL)
          XLB1(MMXR ) = XR(IUL)
          XLB1(MMGMA) = GMA(IUL)
          XLB1(MMPGL) = PGL(IUL)
          XLB1(MMSTE) = STNE(IUL)
C
          XLB1(MMIFS) = IFS(IUL)
          XLB1(MMILS) = ILS(IUL)
          XLB1(MMNED) = NED(IUL)
          XLB1(MMSB1) = ISB1(IUL)
          XLB1(MMSB2) = ISB2(IUL)
          XLB1(MMSTI) = IST(IUL)
          XLB1(MMSTK) = KST(IUL)
C
          call MOVE1   (COP(1,IUL), N    , XLB1(MMCOP))
          call MOVE1   (RHW(1,IUL), N    , XLB1(MMRHW))
          call MOVE1   (CRD(1,IUL), LDLMX, XLB1(MMCRD))
          call MOVE1   (CVW(1,IUL), LDLMX, XLB1(MMCVW))
          call MOVE1   (CSK(1,IUL), LDLMX, XLB1(MMCSK))
          call MOVE1   (CDL(1,IUL), LDLMX, XLB1(MMCDL))
          call MOVE1   (DDL(1,IUL), LDLMX, XLB1(MMDDL))
          call MOVE1   (DWN(1,IUL), LDLMX, XLB1(MMDWN))
          call MOVE1   (XIB(1,IUL), KBTMX, XLB1(MMIXB))
          call MOVE1   (XIR(1,IUL), KRTMX, XLB1(MMIXR))
          call MOVE1   (XIS(1,IUL), KSTMX, XLB1(MMIXS))
C
          call DRECK   (IL, XLB1(MMXR), N, XLB1(MMLRI))
          call KORINA  (N, IU, IL, X(JJRHO), X(JJYBR), X(JJQHI),
     $                  X(JJAW), XLB1)
C
          call LIDMAK  (XLB1,1,XLB2,1,XLB3,1,I)
  100   continue
        call MALLARD   ('ELIDYR')
      end if
C     !END
      call BYE ('ELIDYR')
C
      return
      end
