      subroutine CROWN
     $(X,W,IW,XCBL,XLB1,N,MPROM,IMG)
C
C     Rudolf Loeser, 2006 Feb 27
C---- Computes GTN and line-center mean-TAU.
C     !DASH
      save
C     !DASH
      real*8 DL0, W, X, XCBL, XLB1
      integer IFDDL, IMG, IN, IPHI, IS, IVEC1, IVEC2, IW, JJBDI, JJVXS,
     $        JJXND, JJXNE, KKCAPP, KKMULT, KTAB, LDL, LU, MMCDL, MMCOP,
     $        MMDDL, MMDP, MMDW, MMGTN, MMGTO, MMLAM, MMSTE, MMTAM, MOX,
     $        MPROM, N
      logical lummy1, lummy2
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 59),JJXND)
      equivalence (IZOQ( 44),JJBDI)
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ(129),JJVXS)
C
C---- COBLOCK     as of 2005 Mar 04
      integer     NKKK,MIKLEN,KKK
      parameter   (NKKK=59)
C     (Remember to recompile GERIN when changing NKKK)
      dimension   KKK(NKKK)
      common      /COBLOCK/ MIKLEN, KKK
C     Continuum Data Block components index.
      equivalence (KKK(29),KKCAPP)
      equivalence (KKK( 2),KKMULT)
C
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(45),MMGTO)
      equivalence (MML(51),MMTAM)
      equivalence (MML(17),MMCOP)
      equivalence (MML( 2),MMLAM)
      equivalence (MML(13),MMDW )
      equivalence (MML(61),MMSTE)
      equivalence (MML(12),MMDP )
      equivalence (MML(34),MMDDL)
      equivalence (MML(15),MMGTN)
      equivalence (MML(31),MMCDL)
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
      equivalence (LINKDS(12),LDL  )
C     !DASH
C     !EJECT
      external PLUG, MOVE1, CONMUL, DANAKIL, BLIP, GOLD, WGIVE, NIMBLE,
     $         YMUIR, DEODAR, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               XCBL(Miklen), XLB1(Li1len), IMG(N)
      dimension XCBL(*),      XLB1(*),      IMG(*)
C
      dimension IN(4)
      equivalence
     $(IN( 1),IVEC1 ),(IN( 2),IVEC2),(IN( 3),IPHI  ),(IN( 4),IFDDL )
C
      dimension KTAB(2)
      data KTAB   /24, 25/
C
      data LU,DL0 /0, 0.D0/
C
      call HI ('CROWN')
C     !BEG
C     (Get, and allocate, W allotment)
      call GOLD    (IN, IS, MOX, 'CROWN')
C
C---- Read line-center Continuum Data Block into XCBL
      call PLUG    (XLB1(MMLAM), KTAB, 2, XCBL)
C---- Retrieve total pure absorption
      call MOVE1   (XCBL(KKCAPP), N, XLB1(MMCOP))
      call CONMUL  (XCBL(KKMULT), XLB1(MMCOP), N)
C---- Compute GTO
      call DANAKIL (X, W, IW, LU, N, X(JJXND), X(JJBDI), XLB1(MMDW),
     $              XLB1(MMGTO), W(IVEC1), lummy1, lummy2)
C---- Compute line-center PHI (with mu = 1)
      call NIMBLE  (XLB1(MMSTE), X(JJXNE), W(IFDDL), N)
      call YMUIR   (W, IW, XLB1(MMLAM), DL0, 1, XLB1(MMDP), XLB1(MMDW),
     $              X(JJXNE), X(JJVXS), N, XLB1(MMDDL), W(IFDDL),
     $              XLB1(MMCDL), LDL, MPROM, W(IPHI))
C---- Get final GTN
      call DEODAR  (N, XLB1(MMGTO), XLB1(MMGTN), W(IPHI), XLB1(MMCOP),
     $              lummy1, IMG)
C---- Now compute TAU
      call BLIP    (X, W, N, XLB1(MMCOP), XLB1(MMGTO), XLB1(MMTAM),
     $              IMG, W(IVEC1), W(IVEC2), lummy1, lummy2)
C
C     (Give back W allotment)
      call WGIVE   (W, 'CROWN')
C     !END
      call BYE ('CROWN')
C
      return
      end
