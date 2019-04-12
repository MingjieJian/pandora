      subroutine HUGH
     $(X,IX,W,IW,INDADR,BC,B1,B2,MPROM)
C
C     Rudolf Loeser, 1985 Jul 17
C---- Controls the calculation of PRD terms for Background calculations.
C     (This is version 4 of HUGH.)
C     !DASH
      save
C     !DASH
      real*8 B1, B2, BC, W, X, dummy
      integer IAA, ICE, IDJB, IDL, IDV, IFAB, IFDDL, IFJJ, IFJN, IFO,
     $        IFRD, IGII, IGJN, IGMA, IGRD, IIMG, ILSF, IN, INDADR,
     $        IOVER, IPHC, IPHI, IS, ISAP, ITER, IUU, IV, IVG, IVXI, IW,
     $        IWG, IWS, IX, IXIK, IXJBR, IXK1, IXK2, IXQSF, IXRD, IYRD,
     $        IZRD, JJDDR, JJTE, JJXDR, JJXNC, JJXNE, JJXNU, JN, KTRN,
     $        MMA, MMBS, MMCDL, MMCDW, MMCRD, MMDDL, MMDP, MMDW, MMEP,
     $        MMFE, MMGMA, MMGTN, MMJNU, MMLAM, MMLRI, MMPE, MMSLF,
     $        MMSTE, MMTAM, MMXC, MMXI, MMXP, MMXR, MOX, MPROM, MUX, N,
     $        NDR
      logical ADD, JNU
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(41),NDR)
C
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML( 4),MMCDW)
      equivalence (MML(34),MMDDL)
      equivalence (MML(31),MMCDL)
      equivalence (MML(12),MMDP )
      equivalence (MML(13),MMDW )
      equivalence (MML(44),MMGMA)
      equivalence (MML( 7),MMCRD)
      equivalence (MML(42),MMXC )
      equivalence (MML(43),MMXP )
      equivalence (MML(21),MMEP )
      equivalence (MMP( 6),MMSLF)
      equivalence (MML(23),MMBS )
      equivalence (MML( 3),MMXI )
      equivalence (MML(14),MMA  )
      equivalence (MML(15),MMGTN)
      equivalence (MML( 2),MMLAM)
      equivalence (MML(51),MMTAM)
      equivalence (MML(64),MMLRI)
      equivalence (MMP( 5),MMJNU)
      equivalence (MML(61),MMSTE)
      equivalence (MML(63),MMXR )
      equivalence (MML(19),MMPE )
      equivalence (MML(20),MMFE )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(149),JJXNC)
      equivalence (IZOQ( 46),JJXDR)
      equivalence (IZOQ( 47),JJDDR)
      equivalence (IZOQ( 26),JJXNU)
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
      equivalence (LEST( 2),IOVER)
      equivalence (LEST( 3),ITER )
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
      equivalence (LINKDS(20),KTRN )
      equivalence (LINKDS( 4),ICE  )
C     !DASH
C     !EJECT
      external IMMAKE, POBLANO, CYRENE, DIOCLES, LAUDOMA, WGIVE, YEAST,
     $         CROWN, BABOON, IGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               INDADR(KM), B1(Li1len), B2(Li2len), BC(Miklen)
      dimension INDADR(*),  B1(*),      B2(*),      BC(*)
C
      dimension IN(31)
      equivalence
     $(IN( 1),IFDDL ),(IN( 2),IDL   ),(IN( 3),IV    ),(IN( 4),IPHI  ),
     $(IN( 5),IDV   ),(IN( 6),IAA   ),(IN( 7),IUU   ),(IN( 8),IPHC  ),
     $(IN( 9),IGMA  ),(IN(10),IDJB  ),(IN(11),ISAP  ),(IN(12),IFAB  ),
     $(IN(13),IVXI  ),(IN(14),IXQSF ),(IN(15),IFJN  ),(IN(16),IFJJ  ),
     $(IN(17),IXIK  ),(IN(18),IXRD  ),(IN(19),IYRD  ),(IN(20),IFO   ),
     $(IN(21),IXK1  ),(IN(22),IXK2  ),(IN(23),IGII  ),(IN(24),IVG   ),
     $(IN(25),IWG   ),(IN(26),IFRD  ),(IN(27),IGRD  ),(IN(28),IXJBR ),
     $(IN(29),IGJN  ),(IN(30),ILSF  ),(IN(31),IZRD  )
C
      dimension JN(1)
      equivalence
     $(JN( 1),IIMG  )
C
      data ADD /.false./
C     !EJECT
C
      call HI ('HUGH')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call LAUDOMA (IN, IS,  MOX, 'HUGH')
      call IMMAKE  (JN, IWS, MUX, 'HUGH')
C
C---- Compute DRLIMI
      call POBLANO (N, B1(MMXR), X(JJTE), X(JJXNC), B1(MMLRI))
C---- Get Background data
      JNU = .true.
      if((IOVER*ITER).eq.1) then
        JNU = .false.
      end if
      call YEAST   (INDADR, BC, KTRN, JNU, B2(MMJNU), ADD, dummy,
     $              dummy, dummy)
C---- Set up "Perseus" Line Source Function
      call BABOON  (N, B1, W(ILSF))
C---- Set up GTN and line-center mean-TAU
      call CROWN   (X, W, IW, BC, B1, N, MPROM, IW(IIMG))
C
      call CYRENE  (X, W, IW, N, KTRN, NDR, MPROM,
     $              X(JJXNU), X(JJXDR), X(JJDDR), X(JJXNE),
     $              B1(MMCDW), B1(MMDDL), W(IFDDL), B1(MMSTE),
     $              B1(MMCDL), B1(MMDP), B1(MMDW), B1(MMTAM),
     $              B1(MMGMA), B1(MMCRD), B1(MMXC), B1(MMXP),
     $              B1(MMXR), B1(MMLRI), B1(MMEP), B1(MMPE),
     $              B1(MMFE), B2(MMSLF), B1(MMBS), B1(MMGTN),
     $              W(ILSF), B1(MMLAM), B1(MMXI), B1(MMA),
     $              B2(MMJNU), W(IGMA), W(IDL), W(ISAP),
     $              W(IPHI), W(IFAB), W(IFJN), W(IFJJ), W(IVXI),
     $              W(IXQSF), W(IXK1), W(IXK2), W(IDJB), W(IFRD),
     $              W(IGRD), W(IXRD), W(IZRD), W(IYRD), W(IV),
     $              W(IDV), W(IAA), W(IUU), W(IPHC), W(IXIK),
     $              W(IGII), W(IVG), W(IWG), W(IXJBR), W(IGJN),
     $              W(IFO), IW(IIMG))
C
C---- Update Continuum Data Blocks
      call DIOCLES (INDADR, BC, ICE, KTRN, W(IDL), W(IPHI), B1(MMGTN),
     $              W(IXQSF), W(IVXI), W(IXRD), W(IYRD), W(ILSF))
C
C     (Give back W & IW allotments)
      call WGIVE   (W,  'HUGH')
      call IGIVE   (IW, 'HUGH')
C     !END
      call BYE ('HUGH')
C
      return
      end
