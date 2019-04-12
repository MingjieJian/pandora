      subroutine SWALLOW
     $(X,IX,W,IW,XLB1,XLB2,KTRN,KRJ,KSE,KSEDA)
C
C     Rudolf Loeser, 1984 Oct 23
C---- Controls calculation of Statistical Equilibrium calculation
C     terms, and miscellaneous radiation terms, for MINUET.
C     (This is version 2 of SWALLOW.)
C     !DASH
      save
C     !DASH
      real*8 CRIT, T, W, X, XLB1, XLB2
      integer IBSN, ICE, IEF, IEFN, IEPN, IFN, IFO, IFSAV, IFSLF, IIMG,
     $        IL, IN, IPN, IQESW, IQPSW, IS, IU, IW, IWS, IX, JJALF,
     $        JJBAT, JJBDI, JJBTR, JJCIJ, JJGM, JJRHO, JJSET, JJTE,
     $        JJTR, JJXND, JJXNU, JJYBR, JN, KHED, KLAB, KRJ, KSE,
     $        KSEDA, KTRN, LP, LSFP, LSFT, LU0, LUC, LUG, LUT, METNEW,
     $        METOLD, METSE, MM, MMB, MMBS, MMBTR, MMCDW, MMCND, MMEP,
     $        MMFE, MMLAM, MMPE, MMSB1, MMSB2, MMSLF, MMSN, MMXI, MO,
     $        MOX, MUX, N, NO
      logical ALL, BAD, EDITED, LAST, LILROY, SW
      character METNAM*15
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 25),JJALF)
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ(  7),JJTE )
      equivalence (IZOQ( 39),JJBAT)
      equivalence (IZOQ(  8),JJTR )
      equivalence (IZOQ( 43),JJBTR)
      equivalence (IZOQ( 59),JJXND)
      equivalence (IZOQ( 45),JJRHO)
      equivalence (IZOQ( 13),JJYBR)
      equivalence (IZOQ(145),JJCIJ)
      equivalence (IZOQ( 16),JJGM )
      equivalence (IZOQ( 44),JJBDI)
      equivalence (IZOQ(253),JJSET)
C     !EJECT
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML(19),MMPE )
      equivalence (MML(50),MMSB2)
      equivalence (MML( 2),MMLAM)
      equivalence (MML( 4),MMCDW)
      equivalence (MML( 3),MMXI )
      equivalence (MMP( 6),MMSLF)
      equivalence (MML(49),MMSB1)
      equivalence (MML(20),MMFE )
      equivalence (MML(22),MMB  )
      equivalence (MML(47),MMBTR)
      equivalence (MML(25),MMCND)
      equivalence (MML(67),MMSN )
      equivalence (MML(21),MMEP )
      equivalence (MML(23),MMBS )
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
      equivalence (IQQ(186),IQESW)
      equivalence (IQQ(185),IQPSW)
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
      equivalence (LINKDS( 6),METSE)
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
      equivalence (LINKDS( 4),ICE  )
      equivalence (LINKDS(14),LSFP )
      equivalence (LINKDS(10),LSFT )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
      equivalence (LUNITS( 8),MO   )
C
C---- KNTPEFE     as of 2003 Nov 20
      integer     KNTPF
      parameter   (KNTPF=5)
C     The number of alternative methods for computing PE and FE.
C     (Used in JOAN, SWALLOW.)
C     .
C     !DASH
C     !EJECT
      external LEMON, ZEUS, KRAUT, VICTOR, NEAT, BEAT, UNIFORM, ANKOLE,
     $         VEERY, WHISKEY, TORY, FIMO, KUDU, JOAN, BOUNDLO, SALMON,
     $         FOUR, WGIVE, TIDY, IMMAKE, IGIVE, ZERO1, VODKA, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XLB1(Li1len), XLB2(Li2len)
      dimension XLB1(*),      XLB2(*)
C
      dimension IN(9)
      equivalence
     $(IN( 1),IPN   ),(IN( 2),IFN   ),(IN( 3),IEPN  ),(IN( 4),IEFN  ),
     $(IN( 5),IBSN  ),(IN( 6),IEF   ),(IN( 7),IFSLF ),(IN( 8),IFO   ),
     $(IN( 9),IFSAV )
C
      dimension JN(1)
      equivalence
     $(JN( 1),IIMG  )
C
      dimension T(KNTPF), MM(KNTPF), METNAM(KNTPF), SW(KNTPF)
C
      data CRIT /-9.999D-1/
C
C
      data MM /1, 3, 4, 0, 2/
C
      data METNAM /'NOVA           ', 'COMPLEX/UPPER  ',
     $             'COMPLEX/LOWER  ', 'CHAIN          ',
     $             'VAMOS          '/
C
      data KLAB,KHED,LU0 /2, 1, 0/
C
      call HI ('SWALLOW')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call JOAN   (IN, IS,  MOX, 'SWALLOW')
      call IMMAKE (JN, IWS, MUX, 'SWALLOW')
C
      call ZEUS   (MO, LSFP, LP)
C     !EJECT
      if(KSE.eq.1) then
C----   Compute B, CNDT, BC and SN
        call LEMON    (X, W, IW, X(JJALF), X(JJXNU), X(JJTE),
     $                 X(JJBAT), X(JJTR), X(JJBTR), X(JJSET),
     $                 XLB1(MMB), XLB1(MMBTR), XLB1(MMCND), XLB1(MMSN))
C----   Compute SLF (needed for PRD calculation)
        if(ICE.ne.0) then
          call SALMON (IU, IL, XLB1(MMLAM), XLB1(MMCDW), KTRN,
     $                 XLB1(MMXI), N, X(JJTE), X(JJBDI), W(IFSLF),
     $                 W(IFSAV), IW(IIMG), W(IFO), EDITED, XLB2(MMSLF),
     $                 LU0, KHED, KLAB)
        else
          call ZERO1  (XLB2(MMSLF), N)
        end if
      end if
C
      if(LSFT.ne.2) then
C----   Print a supplementary header
        call KRAUT   (MO, KSE, KSEDA)
C----   Set status descriptor
        LAST = (KSEDA.eq.1).or.(KSE.eq.2)
C----   Set Statistical Equilibrium details output switch
        LILROY = LAST
C----   Set up switches controlling which method(s) to compute
        call VICTOR  (METSE, MO, LAST, SW, KNTPF, ALL)
C----   Compute PN and FN (1 or KNTPF sets)
        call ZERO1   (T, KNTPF)
        call NEAT    (X, IX, W, IW, W(IPN), W(IFN), SW, KNTPF, T, KRJ,
     $                LAST)
C----   Compute EPN, EFN and BSN (1 or KNTPF sets)
        call BEAT    (X, XLB1(MMB), W(IPN), W(IFN), W(IEPN), W(IEFN),
     $                W(IBSN), SW, KNTPF)
C----   Pick proper set
        call UNIFORM (METSE, N, W(IPN), W(IFN), W(IEPN), W(IEFN),
     $                W(IBSN), XLB1(MMPE), XLB1(MMFE), XLB1(MMEP),
     $                W(IEF), XLB1(MMBS), KNTPF)
C----   Print
        call TORY    (LAST, LP, LUG, LUT, LUC)
        call TIDY    (X, IX, W, T, X(JJCIJ), X(JJGM), XLB1(MMPE),
     $                XLB1(MMFE), XLB1(MMEP), W(IEF), XLB1(MMBS),
     $                X(JJBAT), X(JJRHO), X(JJYBR), W(IPN), W(IFN),
     $                W(IEPN), W(IEFN), W(IBSN), LUT, LUG, LUC, ALL,
     $                LILROY, MM, METNAM, KNTPF, KRJ, LAST)
C     !EJECT
        if(LAST.and.(IQESW.gt.0)) then
C----     Test EP
          call VEERY       (XLB1(MMEP), CRIT, N, BAD)
          if(BAD) then
            if(.not.ALL) then
C----         Compute other (KNTPF-1) sets of PN, FN, EPN, EFN, and BSN
              call WHISKEY (METSE, SW, KNTPF)
              call NEAT    (X, IX, W, IW, W(IPN), W(IFN), SW, KNTPF, T,
     $                      KRJ, LAST)
              call BEAT    (X, XLB1(MMB), W(IPN), W(IFN), W(IEPN),
     $                      W(IEFN), W(IBSN), SW, KNTPF)
            end if
C----       Choose new METSE
            METOLD = METSE
            call VODKA     (W(IEPN), CRIT, N, METSE, METNEW, MM, KNTPF)
            if(METNEW.ne.METSE) then
C----         Put new METSE in proper slots
              METSE = METNEW
              call KUDU    (METSE, IU, IL)
C----         Pick new (better) set of results
              call UNIFORM (METSE, N, W(IPN), W(IFN), W(IEPN),
     $                      W(IEFN), W(IBSN), XLB1(MMPE), XLB1(MMFE),
     $                      XLB1(MMEP), W(IEF), XLB1(MMBS), KNTPF)
            end if
C----       Print
            call FIMO      (LP, LUT, LUG, LUC)
            call TIDY      (X, IX, W, T, X(JJCIJ), X(JJGM), XLB1(MMPE),
     $                      XLB1(MMFE), XLB1(MMEP), W(IEF), XLB1(MMBS),
     $                      X(JJBAT), X(JJRHO), X(JJYBR), W(IPN),
     $                      W(IFN), W(IEPN), W(IEFN), W(IBSN), LUT,
     $                      LUG, LUC, .true., LILROY, MM, METNAM,
     $                      KNTPF, KRJ, LAST)
            call ANKOLE    (NO, IU, IL, METSE, METOLD, METNAM, KNTPF)
          end if
        end if
C----   Edit EP
        if(IQPSW.gt.0) then
          call BOUNDLO     (N, XLB1(MMEP), CRIT)
        end if
        if(LAST) then
C----     Checksums
          call FOUR        (XLB1(MMPE), XLB1(MMFE))
        end if
      end if
C
C     (Give back W & IW allotments)
      call WGIVE           (W,  'SWALLOW')
      call IGIVE           (IW, 'SWALLOW')
C     !END
      call BYE ('SWALLOW')
C
      return
      end
