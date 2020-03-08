      subroutine PENNY
     $(X,IX,W,IW,INDADR,BC,B1,B2,STZ,MPROM,KILROY,LAST)
C
C     Rudolf Loeser, 1985 Jul 18
C---- Controls P.R.D. calculations for line source function and
C     emergent intensity profile calculations.
C     (This is version 3 of PENNY.)
C     !DASH
      save
C     !DASH
      real*8 B1, B2, BC, STZ, W, X
      integer IAA, IBBC, IBKPC, IBSIG, IDJB, IDL, IDV, IFAB, IFDDL,
     $        IFJJ, IFJN, IFJR, IFO, IFRD, IGII, IGJN, IGRD, IIMG, IL,
     $        ILSF, IN, INDADR, IPHC, IPHI, IRXI, IS, ISAP, ISF, IU,
     $        IUU, IV, IVG, IVXI, IW, IWG, IWS, IX, IXIK, IXJBR, IXK1,
     $        IXK2, IXQSF, IXRD, IYRD, IZRD, JJDDR, JJTE, JJXDR, JJXNC,
     $        JJXNE, JJXNU, JJZ, JN, KTRN, MMA, MMBS, MMCDL, MMCDW,
     $        MMCRD, MMDDL, MMDP, MMDW, MMEP, MMFE, MMGMA, MMGMI, MMGTN,
     $        MMJNU, MMKCN, MMLAM, MMLRI, MMPE, MMSCN, MMSLF, MMSLR,
     $        MMSN, MMSNU, MMSTE, MMTAM, MMXC, MMXI, MMXP, MMXR, MOX,
     $        MPROM, MUX, N, NDR
      logical ADD, JNU, KILROY, LAST
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
      equivalence (MML(52),MMGMI)
      equivalence (MML( 7),MMCRD)
      equivalence (MML(42),MMXC )
      equivalence (MML(43),MMXP )
      equivalence (MML(21),MMEP )
      equivalence (MML( 3),MMXI )
      equivalence (MML(14),MMA  )
      equivalence (MML(15),MMGTN)
      equivalence (MML( 2),MMLAM)
      equivalence (MML(51),MMTAM)
      equivalence (MMP( 5),MMJNU)
      equivalence (MML(64),MMLRI)
      equivalence (MML(61),MMSTE)
      equivalence (MML(63),MMXR )
      equivalence (MMP( 7),MMSLR)
      equivalence (MML(67),MMSN )
      equivalence (MML(19),MMPE )
      equivalence (MML(20),MMFE )
      equivalence (MML(23),MMBS )
      equivalence (MMP( 3),MMSCN)
      equivalence (MMP( 4),MMKCN)
      equivalence (MMP( 6),MMSLF)
      equivalence (MMP( 2),MMSNU)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(  9),JJXNE)
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ(149),JJXNC)
      equivalence (IZOQ( 46),JJXDR)
      equivalence (IZOQ( 47),JJDDR)
      equivalence (IZOQ( 26),JJXNU)
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
      equivalence (LINKDS(20),KTRN )
C     !DASH
C     !EJECT
      external POBLANO, NUMBAT, BASALT, BABOON, CRETE, YEAST, IMMAKE,
     $         CROWN, MOLLY, ELIS, WGIVE, IGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               B1(Li1len), BC(Miklen), INDADR(KTRN), B2(Li2len),
      dimension B1(*),      BC(*),      INDADR(*),    B2(*),
C
C               STZ(N,2)
     $          STZ(N,*)
C
      dimension IN(36)
      equivalence
     $(IN( 1),IV    ),(IN( 2),IDL   ),(IN( 3),IPHI  ),(IN( 4),IDV   ),
     $(IN( 5),IAA   ),(IN( 6),IUU   ),(IN( 7),IPHC  ),(IN( 8),IFDDL ),
     $(IN( 9),IBSIG ),(IN(10),ISAP  ),(IN(11),IFAB  ),(IN(12),IBBC  ),
     $(IN(13),IFJN  ),(IN(14),IFJR  ),(IN(15),IRXI  ),(IN(16),IFJJ  ),
     $(IN(17),IBKPC ),(IN(18),IXIK  ),(IN(19),IXRD  ),(IN(20),IYRD  ),
     $(IN(21),IXK1  ),(IN(22),IXK2  ),(IN(23),IGII  ),(IN(24),IVG   ),
     $(IN(25),IWG   ),(IN(26),ISF   ),(IN(27),IFRD  ),(IN(28),IGRD  ),
     $(IN(29),IXJBR ),(IN(30),IGJN  ),(IN(31),IFO   ),(IN(32),IVXI  ),
     $(IN(33),IXQSF ),(IN(34),IDJB  ),(IN(35),ILSF  ),(IN(36),IZRD  )
C
      dimension JN(1)
      equivalence
     $(JN( 1),IIMG  )
C
      data JNU,ADD /.true., .true./
C     !EJECT
C
      call HI ('PENNY')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call MOLLY    (IN, IS,  MOX, 'PENNY')
      call IMMAKE   (JN, IWS, MUX, 'PENNY')
C
C---- Compute DRLIMI
      call POBLANO  (N, B1(MMXR), X(JJTE), X(JJXNC), B1(MMLRI))
C---- Get Background data
      call YEAST    (INDADR, BC, KTRN, JNU, B2(MMJNU), ADD, W(IBKPC),
     $               W(IBBC), W(IBSIG))
C---- Get PRD-related Background terms
      call NUMBAT   (N, KTRN, B2(MMJNU), W(IBKPC), W(IBBC), W(IBSIG),
     $               B2(MMSCN), B2(MMKCN))
C---- Set up "Perseus" Line Source Function
      call BABOON   (N, B1, W(ILSF))
C---- Set up GTN and line-center mean-TAU
      call CROWN    (X, W, IW, BC, B1, N, MPROM, IW(IIMG))
C
      call CRETE    (X, W, IW, N, KTRN, NDR, MPROM, X(JJXNU),
     $               X(JJXDR), X(JJDDR), X(JJXNE), B1(MMCDW),
     $               B1(MMDDL), W(IFDDL), B1(MMSTE), B1(MMCDL),
     $               B1(MMDP), B1(MMDW), B1(MMTAM), B1(MMGMA),
     $               B1(MMCRD), B1(MMXC), B1(MMXP), B1(MMXR),
     $               B1(MMLRI), B1(MMEP), B1(MMPE), B1(MMFE),
     $               B2(MMSLF), B1(MMBS), W(ILSF), B1(MMLAM),
     $               B1(MMXI), B1(MMA), B2(MMJNU), B1(MMGMI),
     $               W(IDL), W(ISAP), W(IPHI), W(IFAB), W(IFJN),
     $               W(IFJJ), W(IFJR), W(IRXI), W(IVXI), W(IXQSF),
     $               B2(MMSNU), W(IXK1), W(IXK2), W(IDJB),
     $               W(IFRD), W(IGRD), W(IXRD), W(IZRD), W(IYRD),
     $               B2(MMSLR), W(IV), W(IDV), W(IAA), W(IUU),
     $               W(IPHC), W(IXIK), W(IGII), W(IVG), W(IWG),
     $               W(IXJBR), W(IGJN), W(IFO), IW(IIMG))
C
C---- Save data for Line Source Function calculation
      call ELIS     (KTRN, B1(MMXI), B2(MMJNU), W(IRXI), W(IXRD),
     $               W(IZRD), W(IYRD), W(IBKPC), W(IBBC), W(IBSIG),
     $               W(IFRD), W(IGRD), STZ)
      if(LAST) then
C----   Save JNU for restart run
        call BASALT (KILROY, IU, IL, N, X(JJZ), KTRN, W(IDL),
     $               B2(MMJNU))
      end if
C
C     (Give back W & IW allotments)
      call WGIVE    (W,  'PENNY')
      call IGIVE    (IW, 'PENNY')
C     !END
      call BYE ('PENNY')
C
      return
      end
