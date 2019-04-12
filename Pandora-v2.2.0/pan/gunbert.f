      subroutine GUNBERT
     $(X,W,IW,XLB1,N,NSHL,NRPMX,CODSRW,PHI,CKL,OPAC,WN,VXS,COP,XLM,DL,
     $ XNE,MPROM,FMULT,INDEX,TOPT,XRAY,Z,R1N,WH,Y,MOVING,ILFLX,IMG,DUMP)
C
C     Rudolf Loeser, 1983 Feb 24
C---- Gets PHI, CKL, OPAC, and weight matrices, for all shell rays,
C     for TOTNES.
C     !DASH
      save
C     !DASH
      real*8 CKL, CODSRW, COP, DL, FMULT, OPAC, PHI, R1N, VXS, W, WH,
     $       WN, X, XLB1, XLM, XNE, XRAY, Y, Z
      integer IAA, ICKLR, ICKP, ICPX, IDPX, IDV, IDWX, IEMX, IFDDL,
     $        IFDLX, IGTX, ILFLX, IMG, IN, INDEX, IOPAR, IOPAW, IPHC,
     $        IPHIR, IPHX, IS, ITAUK, ITAUW, IUU, IVP, IVXX, IW, IWHR,
     $        IWNR, IWRK, IXNEX, LDL, MMCDL, MMDDL, MMDP, MMDW, MMGTN,
     $        MMLAM, MMSTE, MOX, MPROM, N, NRPMX, NSHL
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
      external LELIUS, PALLENE, NIMBLE, FAKIR, WGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               WH(N,N,NSHL), CODSRW(NSHL), PHI(N,N,NSHL), Z(N), DL(1),
      dimension WH(*),        CODSRW(*),    PHI(*),        Z(*), DL(*),
C
C               WN(N,N,NSHL), OPAC(N,N,NSHL), XRAY(NRPMX,NSHL), XNE(N),
     $          WN(*),        OPAC(*),        XRAY(*),          XNE(*),
C
C               VXS(N), COP(N), CKL(N,N,NSHL), IMG(N), XLB1(Li1len)
     $          VXS(*), COP(*), CKL(*),        IMG(*), XLB1(*)
C
      dimension IN(25)
      equivalence
     $(IN( 1),IOPAR ),(IN( 2),ICKLR ),(IN( 3),IPHIR ),(IN( 4),IDPX  ),
     $(IN( 5),IDWX  ),(IN( 6),IVXX  ),(IN( 7),ICPX  ),(IN( 8),IEMX  ),
     $(IN( 9),IGTX  ),(IN(10),IPHX  ),(IN(11),IVP   ),(IN(12),IDV   ),
     $(IN(13),IAA   ),(IN(14),IUU   ),(IN(15),ITAUK ),(IN(16),ITAUW ),
     $(IN(17),IOPAW ),(IN(18),IWNR  ),(IN(19),IWHR  ),(IN(20),IFDLX ),
     $(IN(21),IPHC  ),(IN(22),ICKP  ),(IN(23),IXNEX ),(IN(24),IWRK  ),
     $(IN(25),IFDDL )
C     !EJECT
C
      call HI ('GUNBERT')
C     !BEG
C     (Get, and allocate, W allotment)
      call LELIUS  (IN, IS, MOX, 'GUNBERT')
C
      call NIMBLE  (XLB1(MMSTE), XNE, W(IFDDL), N)
      call PALLENE (N, NSHL, NRPMX, CODSRW, PHI, CKL, OPAC, WN, VXS,
     $              XLB1(MMGTN), COP, XLB1(MMLAM), DL, XLB1(MMDP),
     $              XLB1(MMDW), XNE, MPROM, XLB1(MMDDL), W(IFDDL),
     $              XLB1(MMCDL), LDL, FMULT, INDEX, TOPT, XRAY, Z,
     $              R1N, W(IOPAR), W(ICKLR), W(IPHIR), W(IDPX),
     $              W(IDWX), W(IXNEX), W(IFDLX), W(IVXX), W(ICPX),
     $              W(IEMX), W(IGTX), W(IPHX), W(IVP), W(IDV), W(IAA),
     $              W(IUU), W(ITAUK), W(ITAUW), W(IOPAW), W(IWNR), WH,
     $              W(IWHR), Y, MOVING, ILFLX, W(IPHC), W(ICKP), IMG,
     $              W(IWRK), W, IW, DUMP)
C
      call FAKIR   (N, NSHL, WN, CODSRW, W, DUMP)
C
      if(ILFLX.gt.0) then
        call FAKIR (N, NSHL, WH, CODSRW, W, DUMP)
      end if
C
C     (Give back W allotment)
      call WGIVE   (W, 'GUNBERT')
C     !END
      call BYE ('GUNBERT')
C
      return
      end
