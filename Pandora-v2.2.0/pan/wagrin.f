      subroutine WAGRIN
     $(X,W,XLB1,KHED)
C
C     Rudolf Loeser, 1996 Feb 27
C---- Scattering albedo analysis (= LINECOMP).
C     !DASH
      save
C     !DASH
      real*8 W, X, XLB1, XLMS
      integer IBK, IBND, IL, ILFT, IN, IP, IPL, IQLCD, IRGT, IS, ISCON,
     $        ITAUK, IU, IXCBL, IXLMB, IXLMC, IZ, J1, J2, JJBNL, JJBNU,
     $        JJTE, KHED, KLAB, MMB, MMLAM, MMST, MMTAU, MOX, N, NAB,
     $        NB, NC, NCP, NO, NP, jummy
      logical DOIT, DUMP
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(44),NCP)
      equivalence (JZQ(45),NAB)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(160),JJBNL)
      equivalence (IZOQ(161),JJBNU)
      equivalence (IZOQ(  7),JJTE )
C
C---- ELIZA       as of 2006 Feb 14
      integer     MML,LI1LEN,MMP,LI2LEN,MMT,LI3LEN
      dimension   MML(67), MMP(7), MMT(19)
      common      /ELIZA1/ LI1LEN,MML
      common      /ELIZA2/ LI2LEN,MMP
      common      /ELIZA3/ LI3LEN,MMT
C     Line Intensity Data Block components indices.
      equivalence (MML( 2),MMLAM)
      equivalence (MML(16),MMTAU)
      equivalence (MML(46),MMST )
      equivalence (MML(22),MMB  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
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
C
C---- WAGRAM      as of 1997 Nov 19
      real*8      XWGRTC
      integer     IWGRBS
      common      /WAGRAM1/ IWGRBS
      common      /WAGRAM2/ XWGRTC
C     Control parameters for scattering albedo analysis (LINECOMP):
C     IWGRBS - size of wavelength batch, from which 2 are picked,
C     XWGRTC - minimum TAU criterion.
C     .
C     !EJECT
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
      equivalence (IQQ(314),IQLCD)
C     !DASH
      external DELVEN, TERRER, SULMONA, ERNEST, SIGTUNA, CAMBRAI, SKYE,
     $         ROSATE, KALMAR, PICKEM, TURKU, WISMAR, TARTU, WGIVE,
     $         REVAL, LINER, HI, BYE
C
      dimension X(*), W(*)
C
C               XLB1(Li1len)
      dimension XLB1(*)
C
      dimension IN(9)
      equivalence
     $(IN( 1),IXLMC ),(IN( 2),IP    ),(IN( 3),IPL   ),(IN( 4),IXLMB ),
     $(IN( 5),IXCBL ),(IN( 6),ITAUK ),(IN( 7),ISCON ),(IN( 8),IBK   ),
     $(IN( 9),IZ    )
C
      data ILFT,IRGT,KLAB /-1, +1, 3/
C     !EJECT
C
      call HI ('WAGRIN')
C     !BEG
      XLMS = XLB1(MMLAM)
      call DELVEN     (NCP, NAB, X(JJBNL), X(JJBNU), XLMS, IBND, DOIT)
      if(DOIT) then
C       (Get, and allocate, W allotment)
        call TERRER   (IN, IS, MOX, 'WAGRIN')
C
C----   Initialze printout section
        if(KHED.eq.0) then
          KHED = 1
          call ERNEST (NO, KLAB, jummy)
        end if
C----   Set up dump
        DUMP = IQLCD.gt.0
C
C----   Set up S/B for the line
        NP = 0
        call SIGTUNA  (N, XLB1(MMTAU), XLB1(MMB), XLB1(MMST), W(IP),
     $                 NP, XWGRTC)
C----   Set up continuum wavelengths table
        call CAMBRAI  (IBND, W(IXLMC), NC)
        if(DUMP) then
          call KALMAR (IU, IL, XLMS, XLB1(MMTAU), XLB1(MMST),
     $                 XLB1(MMB), N, IBND, W(IXLMC), NC, 'WAGRIN')
        end if
C
C----   Attempt left batch
        call ROSATE    (ILFT, XLMS, W(IXLMC), NC, W(IXLMB), NB, IWGRBS)
        if(DUMP) then
          call TARTU   (ILFT, W(IXLMB), NB)
        end if
        if(NB.gt.0) then
C----     Retrieve continuum data
          call REVAL   (W(IXCBL), W(IXLMB), NB, N, W(ITAUK), W(ISCON),
     $                  W(IBK))
C----     Pick two sets
          call PICKEM  (W(ITAUK), N, NB, XWGRTC, J1, J2)
C----     Save S/B for these two sets
          call SULMONA (J1, J2, W(IP), NP, N, XWGRTC, W(ITAUK),
     $                  W(ISCON), W(IBK))
          if(DUMP) then
            call TURKU (W(ITAUK), W(ISCON), W(IBK), N, NB, J1, J2)
          end if
        end if
C     !EJECT
C----   Attempt right batch
        call ROSATE    (IRGT, XLMS, W(IXLMC), NC, W(IXLMB), NB, IWGRBS)
        if(DUMP) then
          call TARTU   (IRGT, W(IXLMB), NB)
        end if
        if(NB.gt.0) then
C----     Retrieve continuum data
          call REVAL   (W(IXCBL), W(IXLMB), NB, N, W(ITAUK), W(ISCON),
     $                  W(IBK))
C----     Pick two sets
          call PICKEM  (W(ITAUK), N, NB, XWGRTC, J1, J2)
C----     Save S/B for these two sets
          call SULMONA (J1, J2, W(IP), NP, N, XWGRTC, W(ITAUK),
     $                  W(ISCON), W(IBK))
          if(DUMP) then
            call TURKU (W(ITAUK), W(ISCON), W(IBK), N, NB, J1, J2)
          end if
        end if
        if(DUMP) then
          call WISMAR  (W(IP), NP, N, 'WAGRIN')
        end if
C
        if(NP.gt.1) then
C----     Plot
          call SKYE    (NO, IU, IL, N, NP, W(IP), W(IPL), W(IZ),
     $                  X(JJTE))
        else
          call LINER   (5, NO)
          write (NO,100)
  100     format(' ','There are not enough Composite Line wavelengths ',
     $               'for sampling near this line.')
        end if
C       (Give back W allotment)
        call WGIVE     (W, 'WAGRIN')
      end if
C     !END
      call BYE ('WAGRIN')
C
      return
      end
