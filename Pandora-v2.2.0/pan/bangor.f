      subroutine BANGOR
     $(X,W,IW,XLB1,N,LG,VXS,COP,INDEX,XLM,DL,XNE,MPROM,XMU,PHI,CKL,
     $ OPAC,WN,WH,Y,MOVING,ILFLX,IMG,DUMP)
C
C     Rudolf Loeser, 1983 Feb 18
C---- Gets PHI, CKL, OPAC, WN and WH, for SALAMIS.
C     !DASH
      save
C     !DASH
      real*8 CKL, COP, DL, OPAC, PHI, VXS, W, WH, WN, X, XLB1, XLM, XMU,
     $       XNE, Y
      integer IAA, ICKP, IDV, IEMU, IFDDL, ILFLX, IMG, IN, INDEX, IOPAW,
     $        IPHC, IPHP, IS, ITAUK, ITAUW, IUU, IVP, IW, IWRK, LDL, LG,
     $        MMCDL, MMDDL, MMDP, MMDW, MMGTN, MMLAM, MMSTE, MOX, MPROM,
     $        N
      logical DUMP, MOVING
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
      equivalence (LINKDS(12),LDL  )
C     !EJECT
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(34),MMDDL)
      equivalence (MML(31),MMCDL)
      equivalence (MML( 2),MMLAM)
      equivalence (MML(12),MMDP )
      equivalence (MML(13),MMDW )
      equivalence (MML(15),MMGTN)
      equivalence (MML(61),MMSTE)
C     !DASH
      external KAMBER, PHERAE, NIMBLE, WGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               IMG(N), VXS(N), WN(N,N,LG), OPAC(N,N,LG), XLB1(Li1len),
      dimension IMG(*), VXS(*), WN(*),      OPAC(*),      XLB1(*),
C
C               WH(N,N,LG), CKL(N,N,LG), XNE(N), PHI(N,N,LG), XMU(LG),
     $          WH(*),      CKL(*),      XNE(*), PHI(*),      XMU(*),
C
C               COP(N), DL(1)
     $          COP(*), DL(*)
C
      dimension IN(13)
      equivalence
     $(IN( 1),IPHP  ),(IN( 2),IVP   ),(IN( 3),IDV   ),(IN( 4),IAA   ),
     $(IN( 5),IUU   ),(IN( 6),IEMU  ),(IN( 7),ITAUK ),(IN( 8),IOPAW ),
     $(IN( 9),ITAUW ),(IN(10),ICKP  ),(IN(11),IPHC  ),(IN(12),IWRK  ),
     $(IN(13),IFDDL )
C
      call HI ('BANGOR')
C     !BEG
C     (Get, and allocate, W allotment)
      call KAMBER (IN, IS, MOX, 'BANGOR')
C
      call NIMBLE (XLB1(MMSTE), XNE, W(IFDDL), N)
      call PHERAE (X, W, IW, N, VXS, XLB1(MMGTN), W(IPHP), COP, INDEX,
     $             XLB1(MMLAM), DL, XLB1(MMDDL), W(IFDDL),
     $             XLB1(MMCDL), LDL, XLB1(MMDP), XLB1(MMDW), XNE,
     $             MPROM, W(IVP), W(IDV), W(IAA), W(IUU), XMU,
     $             W(IEMU), LG, PHI, CKL, OPAC, WN, WH, Y, MOVING,
     $             ILFLX, W(ITAUK), W(IOPAW), W(ITAUW), IMG, W(IPHC),
     $             W(ICKP), W(IWRK), DUMP)
C
C     (Give back W allotment)
      call WGIVE  (W, 'BANGOR')
C     !END
      call BYE ('BANGOR')
C
      return
      end
