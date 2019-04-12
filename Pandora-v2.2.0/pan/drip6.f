      subroutine DRIP6
     $(NO,N,TAU,S,ERT,IETA,SL,EL,ZL)
C
C     Rudolf Loeser, 2002 Apr 19
C---- Produces a Lyman source Function plot.
C     !DASH
      save
C     !DASH
      real*8 BOT, EL, ERT, ONE, S, SIG, SL, TAU, TEN, TOP, XETA, YLL,
     $       YUL, ZL, dummy
      integer IBEG, IEND, IETA, KNT, KOPT, N, NH, NO, NV
      logical OK
      character NUMERO*1, PERIOD*1, PLUS*1, STAR*1, TIT*10
C     !COM
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
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !DASH
C     !EJECT
      external SHRIMP, KLINEC, BELOWD, ABOVED, MONKEY, KPRINT, KRIGIA,
     $         ABJECT, LOGO, CHROME, KINIT, MAY, ZED, HI, BYE
C
C               S(N), SL(N), ERT(N), EL(N), TAU(N), ZL(N)
      dimension S(*), SL(*), ERT(*), EL(*), TAU(*), ZL(*)
C
      data NV,NH,KOPT /53, 117, 1/
C
      call HI ('DRIP6')
C     !BEG
      if(NO.gt.0) then
        SIG = ZL10SMA
C----   Compute logs
        call LOGO     (S,   N, 1, SIG, SL)
        call LOGO     (ERT, N, 1, SIG, EL)
C----   Establish abscissa
        IBEG = 0
        IEND = 0
        call ZED      (dummy, N, TAU, N, KOPT, IBEG, IEND, ZL, TIT,
     $                 'DRIP6')
C----   Establish ordinate and ordinate limits
        YUL = -ZZLARGE
        YLL = +ZZLARGE
        KNT = IEND-(IBEG-1)
        call CHROME   (KNT, 1, SL(IBEG), 1, SIG, YUL, YLL)
        call CHROME   (KNT, 1, EL(IBEG), 1, SIG, YUL, YLL)
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
          XETA = IETA
          call KLINEC (IMAGE, XETA, BOT, XETA, TOP, STAR, 0)
        end if
C     !EJECT
C----   Enter first: lines; and second: points; into image.
        call SHRIMP   (ZL, N, IBEG, IEND, EL, 1, PLUS,      1, SIG, 2,
     $                 IMAGE)
        call SHRIMP   (ZL, N, IBEG, IEND, SL, 1, PLUS,      1, SIG, 2,
     $                 IMAGE)
        call SHRIMP   (ZL, N, IBEG, IEND, EL, 1, ALPHS( 5), 1, SIG, 1,
     $                 IMAGE)
        call SHRIMP   (ZL, N, IBEG, IEND, SL, 1, ALPHS(19), 1, SIG, 1,
     $                 IMAGE)
C----   Print plot header and image
        call ABJECT   (NO)
        write (NO,100) TIT
  100   format(' ','Plot of log10s of S (=S) and ERT (=E) vs. ',A10)
        call KPRINT   (IMAGE, NO)
C----   Now plot Taus
        call MAY      (NO, ZL, TAU, N, IBEG, IEND, STAR, 0, 0)
      end if
C     !END
      call BYE ('DRIP6')
C
      return
      end
