      subroutine ASILO
     $(N,Z,S,SD,EP,ED,BS,BD,ZL,SL,SDL,EPL,EDL,BSL,BDL,LU)
C
C     Rudolf Loeser, 1998 Aug 12
C---- Plots the S's: "for show" and "for real".
C     !DASH
      save
C     !DASH
      real*8 BD, BDL, BOT, BS, BSL, ED, EDL, EP, EPL, ONE, S, SD, SDL,
     $       SIG, SL, TEN, TOP, YLL, YUL, Z, ZL, dummy
      integer IBEG, IEND, IL, IU, KNT, KOPT, LU, N, NH, NV
      logical OK
      character NUMERO*1, PERIOD*1, PLUS*1, TIT*10
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
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT(11),TEN   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(38),NUMERO)
      equivalence (SYMBS(42),PERIOD)
      equivalence (SYMBS(39),PLUS  )
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
      external SHRIMP, CHROME, BELOWD, ABOVED, MONKEY, KPRINT, KRIGIA,
     $         LOGO, LINER, KINIT, ABJECT, ZED, HI, BYE
C
C               Z(N), S(N), SD(N), ZL(N), SL(N), SDL(N), EP(N), EPL(N),
      dimension Z(*), S(*), SD(*), ZL(*), SL(*), SDL(*), EP(*), EPL(*),
C
C               ED(N), EDL(N), BS(N), BSL(N), BD(N), BDL(N)
     $          ED(*), EDL(*), BS(*), BSL(*), BD(*), BDL(*)
C
      data KOPT /1/
      data NV,NH /52, 117/
C
      call HI ('ASILO')
C     !BEG
      SIG = ZL10SMA
C---- Compute logs
      call LOGO (S,  N, 1, SIG, SL )
      call LOGO (SD, N, 1, SIG, SDL)
      call LOGO (EP, N, 1, SIG, EPL)
      call LOGO (ED, N, 1, SIG, EDL)
      call LOGO (BS, N, 1, SIG, BSL)
      call LOGO (BD, N, 1, SIG, BDL)
C     !EJECT
C---- Establish abscissa
      IBEG = 0
      IEND = 0
      call ZED      (dummy, N, dummy, N, KOPT, IBEG, IEND, ZL, TIT,
     $               'ASILO')
C---- Establish ordinate and ordinate limits
      YUL = -ZZLARGE
      YLL = +ZZLARGE
      KNT = IEND-(IBEG-1)
      call CHROME   (KNT, 1, SL(IBEG),  1, SIG, YUL, YLL)
      call CHROME   (KNT, 1, SDL(IBEG), 1, SIG, YUL, YLL)
      call CHROME   (KNT, 1, EPL(IBEG), 1, SIG, YUL, YLL)
      call CHROME   (KNT, 1, EDL(IBEG), 1, SIG, YUL, YLL)
      call CHROME   (KNT, 1, BSL(IBEG), 1, SIG, YUL, YLL)
      call CHROME   (KNT, 1, BDL(IBEG), 1, SIG, YUL, YLL)
      call BELOWD   (YLL, ONE, BOT)
      call ABOVED   (YUL, ONE, TOP)
C---- Initialize plot image
      call KINIT    (IMAGE, ZL(IBEG), ZL(IEND), BOT, TOP, NV, NH,
     $               NUMERO, OK)
      if(.not.OK) then
C       Abort with message
        call KRIGIA (ZL(IBEG), ZL(IEND), BOT, TOP, NV, NH)
      end if
      if(KOPT.eq.1) then
C----   Enter grid lines
        call MONKEY (IMAGE, ZL(IBEG), ZL(IEND), TEN, BOT, TOP,
     $               PERIOD, 1)
      end if
C---- Enter first: lines; and second: points; into image.
      call SHRIMP   (ZL, N, IBEG, IEND, EDL, 1, PLUS,      1, SIG, 2,
     $               IMAGE)
      call SHRIMP   (ZL, N, IBEG, IEND, EPL, 1, PLUS,      1, SIG, 2,
     $               IMAGE)
      call SHRIMP   (ZL, N, IBEG, IEND, BDL, 1, PLUS,      1, SIG, 2,
     $               IMAGE)
      call SHRIMP   (ZL, N, IBEG, IEND, BSL, 1, PLUS,      1, SIG, 2,
     $               IMAGE)
      call SHRIMP   (ZL, N, IBEG, IEND, SDL, 1, PLUS,      1, SIG, 2,
     $               IMAGE)
      call SHRIMP   (ZL, N, IBEG, IEND, SL,  1, PLUS,      1, SIG, 2,
     $               IMAGE)
      call SHRIMP   (ZL, N, IBEG, IEND, EDL, 1, ALPHS( 6), 1, SIG, 1,
     $               IMAGE)
      call SHRIMP   (ZL, N, IBEG, IEND, EPL, 1, ALPHS( 5), 1, SIG, 1,
     $               IMAGE)
      call SHRIMP   (ZL, N, IBEG, IEND, BDL, 1, ALPHS( 3), 1, SIG, 1,
     $               IMAGE)
      call SHRIMP   (ZL, N, IBEG, IEND, BSL, 1, ALPHS( 2), 1, SIG, 1,
     $               IMAGE)
      call SHRIMP   (ZL, N, IBEG, IEND, SDL, 1, ALPHS(20), 1, SIG, 1,
     $               IMAGE)
      call SHRIMP   (ZL, N, IBEG, IEND, SL,  1, ALPHS(19), 1, SIG, 1,
     $               IMAGE)
C     !EJECT
C---- Print plot header and image
      call ABJECT (LU)
      write (LU,100) TIT,IU,IL
  100 format(' ','Diffusion Analysis: plot of log10s of quantities, ',
     $           'computed with and without the diffusion term GVL, ',
     $           'vs. ',A,T117,'Line (',I2,'/',I2,')'/
     $       ' ','(Option DIFFANA)',10X,'"with": S (=S), B (=BS), E ',
     $           '(=EP); "without": T (=S), C (=BS), F (=EP).')
      call LINER  (1, LU)
      call KPRINT (IMAGE, LU)
C     !END
      call BYE ('ASILO')
C
      return
      end
