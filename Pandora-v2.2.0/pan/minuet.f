      subroutine MINUET
     $(X,IX,W,IW,XLB1,XLB2,XCBL)
C
C     Rudolf Loeser, 1980 May 09
C---- Controls the sub-iterations, which improve RHOs and B-Ratios.
C
C---- XLBL is the Line Intensity data block buffer;
C     XCBL is the Continuum data block buffer.
C     !DASH
      save
C     !DASH
      real*8 TIN, W, X, XCBL, XLB1, XLB2, dummy
      integer IBD, ICE, IED, IL, ILDICO, IN, IS, ISD, ISIT, ISTAU, ISTZ,
     $        ITRN, IU, IVEC, IW, IX, IXKL, IXKT, JJMIJ, JLFLX, JPR,
     $        JPROM, JTRANS, KLIN, KRJ, KSE, KSEDA, KTRN, MO, MOX, MSFT,
     $        MTRANS, NL, NT
      logical LAST, MORE, POUT, PRNT
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
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
      equivalence (LINKDS( 4),ICE  )
      equivalence (LINKDS( 3),KLIN )
      equivalence (LINKDS(20),KTRN )
C     !EJECT
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 5),NT )
      equivalence (JZQ( 2),NL )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ( 16),JJMIJ)
C     !DASH
      external JUNTA, PET, SECOND, LIDGET, TEPEE, WGIVE, BLENNY, TOBOL,
     $         SWALLOW, NUTIME, COGNAC, GRUMP, BELMONT, CRAMP, PERSEUS,
     $         LILY, LIDPUT, REINE, MADGE, TOMBLEN, PHILIP, ZEROI,
     $         HALT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XLB1(Lizlen), XLB2(Li2len), XCBL(Miklen)
      dimension XLB1(*),      XLB2(*),      XCBL(*)
C
      dimension IN(9)
      equivalence
     $(IN( 1),IED   ),(IN( 2),IBD   ),(IN( 3),ISTAU ),(IN( 4),IXKT  ),
     $(IN( 5),IXKL  ),(IN( 6),ISD   ),(IN( 7),ISIT  ),(IN( 8),IVEC  ),
     $(IN( 9),ISTZ  )
C
      call HI ('MINUET')
C     !BEG
C     (Get, and allocate, W allotment)
      call JUNTA   (IN, IS, MOX, 'MINUET')
C
C---- Set statistical equilibrium calculations input switch
      call BELMONT (X, IX, KRJ)
C---- Get updated SA and A* for statistical equilibrium calculations
      call REINE   (X)
C---- Initialize GTN-editing record
      call ZEROI   (IX(JJMIJ), 1, (NL*NL))
C
      PRNT = MO.gt.0
C---- General printout explanation
      call TOMBLEN
C     !EJECT
      ILDICO = 1
      JTRANS = 0
      MTRANS = 0
      do 102 ITRN = 1,NT
        call PET           (ITRN)
        if(KLIN.eq.1) then
C----     Process only radiative transitions
          JTRANS = JTRANS+1
          call SECOND      (TIN)
C----     Read Line Intensity Data Block for this transition
          call LIDGET      (XLB1, 1, XLB2, 1, dummy, 0, ITRN)
C----     Do various initializations
          call MADGE       (X, XLB1, JLFLX, JPROM)
C----     Set up Line Source Function (LSF) calculation controls
          call CRAMP       (JPR, LAST, POUT, KSEDA, KSE)
C
  100     continue
C           Loop over diffusion alternatives (KSEDA = 1 or 2)
C           (i.e. compute SE and LSF both with and without GNV)
            KSE = KSE+1
C----       Compute and print statistical equilibrium (SE) data
            call SWALLOW   (X, IX, W, IW, XLB1, XLB2, KTRN, KRJ,
     $                      KSE, KSEDA)
C
  101       continue
C----         PRD-iterations loop
              JPR = JPR+1
C----         Calculate PRD terms if needed
              call NUTIME  (X, IX, W, IW, ICE, XLB1, XLB2, XCBL,
     $                      W(ISTZ), MTRANS, KTRN, JPROM, JLFLX, JPR,
     $                      LAST, POUT, PRNT)
C             Do remaining set-ups for LSF calculation (including
C             line-center background data, TAUs, and MSFT)
              call GRUMP   (X, IX, W, IW, XLB1, XCBL, JPROM, JTRANS,
     $                      MSFT, W(IXKL), W(IXKT), W(ISTAU))
C----         Line Source Function (LSF)
              call PERSEUS (X, IX, W, IW, XLB1, XLB2, ILDICO, JTRANS,
     $                      JLFLX, MSFT, JPROM, KSE, KSEDA, W(IXKL),
     $                      W(IXKT), W(ISD), W(IED), W(IBD), W(ISTZ))
C----         PRD-iterations administration
              call PHILIP  (X, IX, W, IW, ICE, JPR, MORE, LAST, XLB1,
     $                      W(ISIT), W(IVEC))
            if(MORE) goto 101
C
          if(KSE.lt.KSEDA) goto 100
C
C----     Compute Line Flux Distribution and Radiation Force if needed
          call TOBOL       (X, IX, W, IW, JLFLX, XLB1, XLB2, JPROM)
C----     Rewrite Line Intensity Data Block
          call LIDPUT      (XLB1, 1, XLB2, 1, dummy, 0, ITRN)
C----     Print total time for this transition
          call TEPEE       (IU, IL, TIN)
        end if
  102 continue
C     !EJECT
      if(ILDICO.le.0) then
        MSSLIN(1) = 'A Line Source Function calculation failed.'
        call HALT          ('MINUET', 1)
      end if
C
C---- Final processing of "Artificial TAU"
      call BLENNY (X)
C---- Fancy Tau-scales printing (all transitions)
      call COGNAC (X, W, IW, W(ISTAU))
C
C---- Recompute B-ratios, and get final RHOs and JBARs
C     (for all transitions)
      call LILY   (X, IX, W, IW, XLB1)
C
C     (Give back W & IW allotments)
      call WGIVE  (W, 'MINUET')
C     !END
      call BYE ('MINUET')
C
      return
      end
