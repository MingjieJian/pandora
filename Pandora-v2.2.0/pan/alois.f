      subroutine ALOIS
     $(KOPT,N,Z,S,B,XJBR,ST,TAU,TAUM,ZL,SL,BL,XJBRL,STL,RULED,RULP,NO)
C
C     Rudolf Loeser, 1982 Jun 14
C---- Produces a Line Source Function plot.
C     (This is version 2 of ALOIS.)
C     !DASH
      save
C     !DASH
      real*8 B, BL, BOT, ONE, RULED, RULP, S, SIG, SL, ST, STL, TAU,
     $       TAUM, TEN, TOP, XJBR, XJBRL, YLL, YUL, Z, ZL
      integer IBEG, IEND, IL, IU, KNT, KOPT, N, NH, NO, NV
      logical OK
      character BLANK*1, NUMERO*1, PERIOD*1, PLUS*1, QQ*50, STAR*1,
     $          TIT*10
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
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !EJECT
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(38),NUMERO)
      equivalence (SYMBS(42),PERIOD)
      equivalence (SYMBS(45),STAR  )
      equivalence (SYMBS(39),PLUS  )
      equivalence (SYMBS(43),BLANK )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT(11),TEN   )
C
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C     !DASH
C     !EJECT
      external SHRIMP, WRYBILL, BELOWD, ABOVED, MONKEY, KPRINT, KRIGIA,
     $         LOGO, CHROME, KINIT, MAY, ZED, HI, BYE
C
C               ZL(N), SL(N), BL(N), XJBRL(N), Z(N), S(N), B(N), ST(N),
      dimension ZL(*), SL(*), BL(*), XJBRL(*), Z(*), S(*), B(*), ST(*),
C
C               XJBR(N), TAU(N), STL(N), TAUM(N), RULED(N), RULP(N)
     $          XJBR(*), TAU(*), STL(*), TAUM(*), RULED(*), RULP(*)
C
      data NV,NH /50, 117/
C
      call HI ('ALOIS')
C     !BEG
      if(NO.gt.0) then
        SIG = ZL10SMA
C----   Compute logs
        call LOGO     (S,    N, 1, SIG, SL   )
        call LOGO     (B,    N, 1, SIG, BL   )
        call LOGO     (XJBR, N, 1, SIG, XJBRL)
        call LOGO     (ST,   N, 1, SIG, STL  )
C----   Establish abscissa
        IBEG = 0
        IEND = 0
        call ZED      (Z, N, TAUM, N, KOPT, IBEG, IEND, ZL, TIT,
     $                 'ALOIS')
C----   Establish ordinate and ordinate limits
        YUL = -ZZLARGE
        YLL = +ZZLARGE
        KNT = IEND-(IBEG-1)
        call CHROME   (KNT, 1, SL(IBEG),    1, SIG, YUL, YLL)
        call CHROME   (KNT, 1, BL(IBEG),    1, SIG, YUL, YLL)
        call CHROME   (KNT, 1, XJBRL(IBEG), 1, SIG, YUL, YLL)
        call CHROME   (KNT, 1, STL(IBEG),   1, SIG, YUL, YLL)
        call BELOWD   (YLL, ONE, BOT)
        call ABOVED   (YUL, ONE, TOP)
C----   Initialize plot image
        call KINIT    (IMAGE, ZL(IBEG), ZL(IEND), BOT, TOP, NV, NH,
     $                 NUMERO, OK)
        if(.not.OK) then
C         Abort with message
          call KRIGIA (ZL(IBEG), ZL(IEND), BOT, TOP, NV, NH)
        end if
        if(KOPT.eq.1) then
C----     Enter grid lines
          call MONKEY (IMAGE, ZL(IBEG), ZL(IEND), TEN, BOT, TOP,
     $                 PERIOD, 1)
        end if
C     !EJECT
C----   Enter first: lines; and second: points; into image.
        call SHRIMP   (ZL, N, IBEG, IEND, BL,    1, PLUS,      1, SIG,
     $                 2, IMAGE)
        call SHRIMP   (ZL, N, IBEG, IEND, XJBRL, 1, PLUS,      1, SIG,
     $                 2, IMAGE)
        call SHRIMP   (ZL, N, IBEG, IEND, SL,    1, PLUS,      1, SIG,
     $                 2, IMAGE)
        call SHRIMP   (ZL, N, IBEG, IEND, STL,   1, PLUS,      1, SIG,
     $                 2, IMAGE)
        call SHRIMP   (ZL, N, IBEG, IEND, BL,    1, ALPHS( 2), 1, SIG,
     $                 1, IMAGE)
        call SHRIMP   (ZL, N, IBEG, IEND, XJBRL, 1, ALPHS(10), 1, SIG,
     $                 1, IMAGE)
        call SHRIMP   (ZL, N, IBEG, IEND, SL,    1, ALPHS(19), 1, SIG,
     $                 1, IMAGE)
        call SHRIMP   (ZL, N, IBEG, IEND, STL,   1, ALPHS(20), 1, SIG,
     $                 1, IMAGE)
C----   Set up population-inversion indicator, and enter into image
        call WRYBILL  (N, TOP, SIG, RULED, RULP, OK)
        if(OK) then
          call SHRIMP (ZL, N, IBEG, IEND, RULP, 1, PLUS,      1, SIG,
     $                 2, IMAGE)
          call SHRIMP (ZL, N, IBEG, IEND, RULP, 1, ALPHS(17), 1, SIG,
     $                 1, IMAGE)
          QQ = '(Q at top shows place of population inversion.)'
        else
          QQ = BLANK
        end if
C
C----   Print plot header and image
        write (NO,100) TIT,IU,IL,QQ
  100   format(' ','Plot of log10s of ST (=T), S (=S), B (=B) and ',
     $             'Jbar (=J) vs. ',A10,43X,'For line (',I2,'/',I2,')'/
     $         ' ','(S is the line source function, ST is the total ',
     $             'source function.) ',A)
        call KPRINT   (IMAGE, NO)
C----   Now plot Taus
        call MAY      (NO, ZL, TAU, N, IBEG, IEND, STAR,  IU,  IL)
        call MAY      (NO, ZL,TAUM, N, IBEG, IEND, STAR, -IU, -IL)
        write (NO,101)
  101   format(' ','    TAU-M = Mean Optical Depth ',
     $             '(like TAU, but with PHI = 1).')
      end if
C     !END
      call BYE ('ALOIS')
C
      return
      end
