      subroutine FROLLO
     $(X,W,IW,XLB1,N,MRR,PHI,CKL,OPAC,WN,VXS,COP,XLM,DL,XNE,MPROM,
     $ FMULT,INDEX,TOPT,XRAY,EMU,WH,Y,MOVING,ILFLX,IMG,DUMP)
C
C     Rudolf Loeser, 1983 Feb 23
C---- Gets PHI, CKL, OPAC, and weight matrices, for all disk rays,
C     for TOTNES.
C     !DASH
      save
C     !DASH
      real*8 CKL, COP, DL, EMU, FMULT, OPAC, PHI, VXS, W, WH, WN, X,
     $       XLB1, XLM, XNE, XRAY, Y
      integer IAA, ICKP, IDV, IFDDL, ILFLX, IMG, IN, INDEX, IOPAW, IPHC,
     $        IPHP, IS, ITAUK, ITAUW, IUU, IVP, IW, IWRK, LDL, MMCDL,
     $        MMDDL, MMDP, MMDW, MMGTN, MMLAM, MMSTE, MOX, MPROM, MRR,
     $        N
      logical DUMP, MOVING, TOPT
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
      external LIVIUS, PYDNA, NIMBLE, WGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               OPAC(N,N,MRR), PHI(N,N,MRR), CKL(N,N,MRR), XRAY(N,MRR),
      dimension OPAC(*),       PHI(*),       CKL(*),       XRAY(*),
C
C               WN(N,N,MRR), WH(N,N,MRR), VXS(N), XLB1(Li1len), IMG(N),
     $          WN(*),       WH(*),       VXS(*), XLB1(*),      IMG(*),
C
C               COP(N), XNE(N), DL(1), EMU(N,MRR)
     $          COP(*), XNE(*), DL(*), EMU(*)
C
      dimension IN(12)
      equivalence
     $(IN( 1),IVP   ),(IN( 2),IDV   ),(IN( 3),IAA   ),(IN( 4),IUU   ),
     $(IN( 5),IPHP  ),(IN( 6),ITAUK ),(IN( 7),IOPAW ),(IN( 8),ITAUW ),
     $(IN( 9),ICKP  ),(IN(10),IPHC  ),(IN(11),IWRK  ),(IN(12),IFDDL )
C
      call HI ('FROLLO')
C     !BEG
C     (Get, and allocate, W allotment)
      call LIVIUS (IN, IS, MOX, 'FROLLO')
C
      call NIMBLE (XLB1(MMSTE), XNE, W(IFDDL), N)
      call PYDNA  (N, MRR, PHI, CKL, OPAC, WN, VXS, XLB1(MMGTN), COP,
     $             XLB1(MMLAM), DL, XLB1(MMDDL), W(IFDDL),
     $             XLB1(MMCDL), LDL, XLB1(MMDP), XLB1(MMDW), XNE,
     $             MPROM, FMULT, INDEX, TOPT, XRAY, EMU, W(IVP),
     $             W(IDV), W(IAA), W(IUU), W(IPHP), W(ITAUK),
     $             W(IOPAW), W(ITAUW), WH, Y, MOVING, ILFLX, W(IPHC),
     $             W(ICKP), IMG, W(IWRK), W, IW, DUMP)
C
C     (Give back W allotment)
      call WGIVE  (W, 'FROLLO')
C     !END
      call BYE ('FROLLO')
C
      return
      end
