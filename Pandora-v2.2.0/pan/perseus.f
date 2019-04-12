      subroutine PERSEUS
     $(X,IX,W,IW,XLB1,XLB2,ILDICO,JTRANS,ILFLX,MSFT,MPROM,KSE,KSEDA,
     $ XKL,XKT,SD,ED,BD,STZ)
C
C     Rudolf Loeser, 2004 May 10
C     RL/SGK revised Mar 22 2014 
C---- Supervises Source Function calculation, for current transition.
C
C     (This is version 2 of PERSEUS.)
C     !DASH
      save
C     !DASH
      real*8 BD, ED, SD, STZ, W, X, XKL, XKT, XLB1, XLB2, YCONT, dummy
      integer IANT, IBA, IBF, IBLOCK, IC, IDDL, IDEL, IDLC, IEXT, IFDDL,
     $        IIMG, ILDICO, ILFLX, IN, IOMD, IPA, IPB, IPD, IPG, IPQ,
     $        IQEXA, IRHOK, IRULED, IS, ISB1, ISB2, ISL, ITI, IUNT,
     $        IVEC, IW, IWS, IX, JJCDK, JJCMU, JJCSH, JJFRS, JJKSR,
     $        JJMDK, JJMSH, JJOML, JJP, JJVSB, JJVXS, JJWDK, JJWMU,
     $        JJWSH, JJXDK, JJXMU, JJXND, JJXNE, JJXSH, JJYCO, JJZ, JN,
     $        JTRANS, KSE, KSEDA, KTRN, MEDUSA, MMA, MMAW, MMB, MMBC,
     $        MMBS, MMBTR, MMCDL, MMCDW, MMCND, MMCOP, MMCSF, MMDDL,
     $        MMDL, MMDP, MMDW, MMEP, MMFND, MMFXI, MMGMA, MMGMI, MMGTN,
     $        MMJBC, MMJBR, MMKCN, MMLAM, MMLRI, MMRHO, MMS, MMSB1,
     $        MMSB2, MMSCN, MMSN, MMST, MMSTE, MMTAM, MMTAU, MMXC, MMXI,
     $        MMXP, MMXR, MMY, MO, MOX, MPROM, MSFT, MUX, N, jummy
      logical DMP0, DMP1, DMP2, EDITS, REAL
      character LINE*45
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(138),JJFRS)
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ(129),JJVXS)
      equivalence (IZOQ(130),JJXMU)
      equivalence (IZOQ(131),JJCMU)
      equivalence (IZOQ(167),JJWMU)
      equivalence (IZOQ( 40),JJXSH)
      equivalence (IZOQ( 60),JJMSH)
      equivalence (IZOQ(109),JJCSH)
      equivalence (IZOQ(169),JJWSH)
      equivalence (IZOQ(132),JJXDK)
      equivalence (IZOQ(133),JJMDK)
      equivalence (IZOQ(134),JJCDK)
      equivalence (IZOQ(168),JJWDK)
      equivalence (IZOQ( 59),JJXND)
      equivalence (IZOQ( 76),JJYCO)
      equivalence (IZOQ(  3),JJOML)
      equivalence (IZOQ( 27),JJP  )
      equivalence (IZOQ(153),JJKSR)
      equivalence (IZOQ(172),JJVSB)
C     !EJECT
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
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
      equivalence (IQQ(169),IQEXA)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !EJECT
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(42),MMXC )
      equivalence (MML(43),MMXP )
      equivalence (MML(44),MMGMA)
      equivalence (MML(52),MMGMI)
      equivalence (MML( 2),MMLAM)
      equivalence (MML( 4),MMCDW)
      equivalence (MML( 5),MMY  )
      equivalence (MML(16),MMTAU)
      equivalence (MML( 3),MMXI )
      equivalence (MML(59),MMDL )
      equivalence (MML(14),MMA  )
      equivalence (MML(12),MMDP )
      equivalence (MML(13),MMDW )
      equivalence (MML(15),MMGTN)
      equivalence (MML(17),MMCOP)
      equivalence (MML(18),MMCSF)
      equivalence (MML(23),MMBS )
      equivalence (MML(21),MMEP )
      equivalence (MML(25),MMCND)
      equivalence (MML(26),MMS  )
      equivalence (MML(46),MMST )
      equivalence (MML(28),MMRHO)
      equivalence (MML(27),MMJBR)
      equivalence (MML(11),MMFND)
      equivalence (MML(34),MMDDL)
      equivalence (MML(31),MMCDL)
      equivalence (MMP( 3),MMSCN)
      equivalence (MMP( 4),MMKCN)
      equivalence (MML(61),MMSTE)
      equivalence (MML(64),MMLRI)
      equivalence (MML(63),MMXR )
      equivalence (MML(65),MMAW )
      equivalence (MML(67),MMSN )
      equivalence (MML(48),MMFXI)
      equivalence (MML(36),MMJBC)
      equivalence (MML(51),MMTAM)
      equivalence (MML(24),MMBC )
      equivalence (MML(22),MMB  )
      equivalence (MML(49),MMSB1)
      equivalence (MML(50),MMSB2)
      equivalence (MML(47),MMBTR)
C     !EJECT
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
      equivalence (LINKDS(20),KTRN )
C     !DASH
C     !EJECT
      external  HERMES, APATITE, PILAF, LIMPOPO, ORION, TOMBOT, NIMBLE,
     $          DIANA, BOTTOM, LOVOA, LANA, FAZZ, PSHAW, WGIVE, IMMAKE,
     $          IGIVE, HI, BYE
      intrinsic min
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XLB1(Li1len), XLB2(Li2len), XKL(N), XKT(N), STZ(N,2),
      dimension XLB1(*),      XLB2(*),      XKL(*), XKT(*), STZ(N,*),
C
C               SD(N), ED(N), BD(N)
     $          SD(*), ED(*), BD(*)
C
c-was-dimension IN(20)-incorrect
      dimension IN(21)
      equivalence
     $(IN( 1),IBF   ),(IN( 2),IBA   ),(IN( 3),IDEL  ),(IN( 4),IUNT  ),
     $(IN( 5),IVEC  ),(IN( 6),IBLOCK),(IN( 7),IDLC  ),(IN( 8),ITI   ),
     $(IN( 9),IRHOK ),(IN(10),IFDDL ),(IN(11),IC    ),(IN(12),IRULED),
     $(IN(13),IANT  ),(IN(14),IPA   ),(IN(15),IPG   ),(IN(16),IPB   ),
     $(IN(17),IOMD  ),(IN(18),IEXT  ),(IN(19),IPQ   ),(IN(20),IPD   ),
     $(IN(21),ISL   )
C
      dimension JN(1)
      equivalence
     $(JN( 1),IIMG  )
C
      call HI ('PERSEUS')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call LANA     (IN, IS,  MOX, 'PERSEUS')
      call IMMAKE   (JN, IWS, MUX, 'PERSEUS')
C
      REAL = (KSEDA.eq.1).or.(KSE.eq.2)
C---- Compute ratio of number densities (for LSF graph)
      call APATITE  (N, X(JJXND), X(JJP), W(IRULED))
C
C---- Write heading
      call LOVOA    (MO, XLB1(MMXC), XLB1(MMXP), XLB1(MMXR),
     $               XLB1(MMGMA), XLB1(MMLAM), X(JJVXS), MSFT, KTRN,
     $               MPROM)
 
      if(MSFT.eq.2) then
C----   Take Escape Probability shortcut
        ISB1 = XLB1(MMSB1)
        ISB2 = XLB1(MMSB2)
        call HERMES (X, IX, W, IW, XLB1(MMTAU), XLB1(MMRHO), W(IRHOK),
     $               XLB1(MMSN), XLB1(MMS), XLB1(MMST), XLB1(MMJBR),
     $               XLB1(MMB), XLB1(MMEP), W(IDEL), XLB1(MMBS),
     $               W(IBA), W(IBF), XLB1(MMFND), X(JJZ), X(JJXND),
     $               X(JJVSB), XLB1(MMFXI), XLB1(MMJBC), XLB1(MMDP),
     $               XLB1(MMDW), X(JJFRS), MSFT, ISB1, ISB2,
     $               XLB1(MMCOP), XLB1(MMBTR), XLB1(MMBC), XLB1(MMCND),
     $               XLB1(MMCSF), XKL, XKT, XLB1(MMTAM), W(IVEC),
     $               W(IRULED), IW(IIMG))
      else
C     !EJECT
C
C----   Use "full" (MSFT=0) or "direct" (MSFT=1) solution
C
C----   Set up dumps
        call PILAF   (REAL, DMP0, DMP1, DMP2)
C
        call NIMBLE  (XLB1(MMSTE), X(JJXNE), W(IFDDL), N)
        call LIMPOPO (YCONT, dummy, X(JJYCO), X(JJOML))
C
C----   Compute frequency/angle-integrated terms
        if(IQEXA.le.0) then
C----     Stationary atmosphere
          call DIANA (X, IX, W, IW, W(IBLOCK), ILFLX, MPROM,
     $                XLB1(MMY), XLB1(MMTAU), XLB1(MMDL), KTRN,
     $                XLB1(MMDP), XLB1(MMDW), XLB1(MMDDL),
     $                XLB1(MMCDL), XLB1(MMGTN), XLB1(MMCOP),
     $                XLB1(MMBC), X(JJZ), XLB1(MMXI), XLB1(MMA),
     $                X(JJVXS), IW(IIMG), XLB2(MMSCN),XLB2(MMKCN),
     $                X(JJXNE), W(IFDDL), MEDUSA, DMP0, DMP1, DMP2,
     $                KSE, KSEDA, W(IC), W(IEXT), W(IANT), XLB1(MMAW),
     $                W(IPA), W(IPG), W(IPB), W(IOMD), W(IDLC),
     $                W(IPQ), W(IPD), XLB1(MMCSF), XLB1(MMB),
     $                XLB1(MMBTR))
        else
C----     Expaanding atmosphere
          call ORION (X, IX, W, IW, W(IBLOCK), ILFLX, MPROM,
     $                XLB1(MMLAM), XLB1(MMY), XLB1(MMDL), KTRN,
     $                XLB1(MMDP), XLB1(MMDW), XLB1(MMDDL),
     $                XLB1(MMCDL), XLB1(MMGTN), XLB1(MMCOP),
     $                XLB1(MMBC), X(JJZ), X(JJXMU), X(JJCMU),
     $                X(JJWMU), X(JJXSH), X(JJKSR), X(JJMSH),
     $                X(JJCSH), X(JJWSH), X(JJXDK), X(JJMDK),
     $                X(JJCDK), X(JJWDK), X(JJVXS), XLB1(MMXI),
     $                XLB1(MMA), IW(IIMG), XLB2(MMSCN), XLB2(MMKCN),
     $                X(JJXNE), W(IFDDL), MEDUSA, DMP0, DMP1, DMP2,
     $                KSE, KSEDA, W(IEXT), W(IANT), XLB1(MMAW),
     $                W(IPA), W(IPG), W(IPB), W(IOMD), W(IDLC),
     $                W(IPQ), W(IPD), XLB1(MMCSF), XLB1(MMB),
     $                XLB1(MMBTR))
        end if
C     !EJECT
C----   Compute Source Function, Net Radiative Bracket, Mean Intensity
        call BOTTOM (W, IW, N, DMP1, MSFT, XLB1(MMCDW), XLB1(MMDW),
     $               XLB1(MMCND), XLB1(MMEP), XLB1(MMBS), XLB1(MMSN),
     $               IW(IIMG), W(IANT), W(IOMD), W(IDLC), W(IPA),
     $               W(IPG), W(IPB), W(IPQ), W(IPD), W(ITI),
     $               XLB1(MMFND), W(IDEL), W(IUNT), IDDL, W(IBF),
     $               W(IBA), XLB1(MMS), EDITS, XLB1(MMJBR),
     $               XLB1(MMRHO), MEDUSA)
C----   Compute total source function
        call FAZZ   (N, XKL, XLB1(MMS), XLB1(MMCOP), XLB1(MMCSF),
     $               XKT, XLB1(MMEP), STZ, W(ISL), LINE, W(IVEC),
     $               XLB1(MMST))
C----   Display results
        call PSHAW  (MO, W, N, XLB1(MMEP), W(IDEL), XLB1(MMB),
     $               W(IBF), XLB1(MMBS), W(IBA), XLB1(MMS),
     $               XLB1(MMST), XLB1(MMTAU), XLB1(MMRHO), W(IANT),
     $               W(ITI), W(IUNT), XLB1(MMJBR), XLB1(MMFND),
     $               X(JJZ), XLB1(MMY), YCONT, X(JJFRS), MSFT, jummy,
     $               jummy, XLB1(MMGMI), XLB1(MMGMA), XLB1(MMLRI),
     $               XLB1(MMXR), XLB1(MMTAM), KSE, KSEDA, SD, ED, BD,
     $               W(IRULED), XLB1(MMSN), XLB1(MMAW), IDDL, XKL,
     $               XKT, XLB1(MMCOP), XLB1(MMCSF), W(ISL),
     $               XLB1(MMBC), X(JJXND), XLB1(MMCND), LINE)
C
        ILDICO = min(ILDICO,MEDUSA)
      end if
C
C---- Final processing
      call TOMBOT   (REAL, JTRANS, X, XLB1)
C
C     (Give back W & IW allotments)
      call WGIVE    (W,  'PERSEUS')
      call IGIVE    (IW, 'PERSEUS')
C     !END
      call BYE ('PERSEUS')
C
      return
      end
