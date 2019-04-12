      subroutine ALLOD
     $(X,IX,W,IW,XLB1,KHED)
C
C     Rudolf Loeser, 1980 Aug 20
C---- Controls LTE Line Data calculation.
C     !DASH
      save
C     !DASH
      real*8 W, X, XLB1
      integer IABDL, IBDS, IFDDL, IFXI, IGTO, IIMG, IL, IN, IS, ITAUM,
     $        IU, IW, IWS, IX, IXKL, IXKT, IXNDS, IXNKS, JJALF, JJBAT,
     $        JJTE, JJVXS, JJXNE, JJXNU, JN, JPROM, KHED, LUL, LUT,
     $        MMCDL, MMCOP, MMDDL, MMDP, MMDW, MMGTN, MMGTS, MMLAM, MMS,
     $        MMSS, MMSTE, MMTAU, MMTS, MOX, MUX, N, NL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(129),JJVXS)
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ( 25),JJALF)
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ( 39),JJBAT)
      equivalence (IZOQ(  9),JJXNE)
C
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(15),MMGTN)
      equivalence (MML(17),MMCOP)
      equivalence (MML(12),MMDP )
      equivalence (MML(13),MMDW )
      equivalence (MML(34),MMDDL)
      equivalence (MML(31),MMCDL)
      equivalence (MML( 2),MMLAM)
      equivalence (MML(32),MMTS )
      equivalence (MML(33),MMSS )
      equivalence (MML(16),MMTAU)
      equivalence (MML(35),MMGTS)
      equivalence (MML(26),MMS  )
      equivalence (MML(61),MMSTE)
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
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
C     !DASH
      external MUTE, CELIA, LINER, NUMB, ONE1, VULTURE, PLANK, PICCOLO,
     $         NIMBLE, GOBY, WGIVE, IMMAKE, IGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XLB1(Lizlen)
      dimension XLB1(*)
C
      dimension IN(10)
      equivalence
     $(IN( 1),IXNDS ),(IN( 2),IBDS  ),(IN( 3),IFXI  ),(IN( 4),ITAUM ),
     $(IN( 5),IABDL ),(IN( 6),IXNKS ),(IN( 7),IFDDL ),(IN( 8),IXKT  ),
     $(IN( 9),IXKL  ),(IN(10),IGTO  )
C
      dimension JN(1)
      equivalence
     $(JN( 1),IIMG  )
C     !EJECT
C
      call HI ('ALLOD')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call MUTE    (IN, IS,  MOX, 'ALLOD')
      call IMMAKE  (JN, IWS, MUX, 'ALLOD')
C
C---- Set up output
      call CELIA   (KHED, LUT, LUL)
C---- Get LTE number densities and departure coefficients
      call ONE1    (W(IBDS), (N*NL))
      call NUMB    (X, W, W(IBDS), 0, W(IABDL), W(IXNKS), W(IXNDS),
     $              IX(IIMG))
C---- Adjust Hydrogen Stark broadening switch
      call GOBY    (XLB1, JPROM)
C---- Compute depth-dependent DDL multiplier
      call NIMBLE  (XLB1(MMSTE), X(JJXNE), W(IFDDL), N)
C---- Get Optical Depth (also GTN, FR)
      call VULTURE (X, IX, W, IW, W(IXNDS), W(IBDS), W(IGTO),
     $              XLB1(MMGTS), W(IFXI), XLB1(MMCOP), XLB1(MMDP),
     $              XLB1(MMDW), JPROM, X(JJXNE), XLB1(MMDDL),
     $              W(IFDDL), XLB1(MMCDL), X(JJVXS), XLB1(MMLAM),
     $              XLB1(MMTS), W(ITAUM), W(IXKL), W(IXKT), LUT)
C---- Get Line Source Function (i.e. Planck function)
      call PLANK   (IU, IL, N, X(JJXNU), X(JJALF), X(JJTE), X(JJBAT),
     $              XLB1(MMSS))
C---- Print
      call PICCOLO (LUL, XLB1(MMCOP), XLB1(MMTAU), XLB1(MMGTN),
     $              XLB1(MMS), XLB1(MMTS), XLB1(MMGTS), XLB1(MMSS))
C
C     (Give back W & IW allotmwnts)
      call WGIVE   (W,  'ALLOD')
      call IGIVE   (IW, 'ALLOD')
C     !END
      call BYE ('ALLOD')
C
      return
      end
