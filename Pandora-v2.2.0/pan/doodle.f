      subroutine DOODLE
     $(HEAD,N,Z,S,B,BHS,XJNU,TAU,ZL,SL,BL,BHSL,XJNUL,NO)
C
C     Rudolf Loeser, 1973 Feb 06
C---- Produces a Continuum Source Function plot.
C     !DASH
      save
C     !DASH
      real*8 B, BHS, BHSL, BL, BOT, ONE, S, SIG, SL, TAU, TOP, XJNU,
     $       XJNUL, YLL, YUL, Z, ZL
      integer IBEG, IEND, KNT, N, NH, NO, NV
      logical GOOD
      character HEAD*12, NUMERO*1, STAR*1, TIT*10
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(38),NUMERO)
      equivalence (SYMBS(45),STAR  )
C
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
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
      external KRIGIA, CHROME, BELOWD, ABOVED, ZED, KINIT, SHRIMP, MAY,
     $         ABJECT, LINER, KPRINT, LOGO, HI, BYE
C
C               ZL(N), TAU(N), BL(N), BHSL(N), XJNUL(N), Z(N), XJNU(N),
      dimension ZL(*), TAU(*), BL(*), BHSL(*), XJNUL(*), Z(*), XJNU(*),
C
C               B(N), BHS(N), S(N), SL(N)
     $          B(*), BHS(*), S(*), SL(*)
C
      data NV,NH /51, 117/
C
      call HI ('DOODLE')
C     !BEG
      if(NO.gt.0) then
C
        SIG = ZL10SMA
C----   Compute logs
        call LOGO   (S,    N, 1, SIG, SL   )
        call LOGO   (B,    N, 1, SIG, BL   )
        call LOGO   (BHS,  N, 1, SIG, BHSL )
        call LOGO   (XJNU, N, 1, SIG, XJNUL)
C
C----   Establish Z points
        IBEG = 0
        IEND = 0
        call ZED    (Z, N, TAU, N, 0, IBEG, IEND, ZL, TIT, 'DOODLE')
C
C----   Find ordinate limits
        YUL = -ZZLARGE
        YLL = +ZZLARGE
        KNT =  IEND-(IBEG-1)
        call CHROME (KNT, 1, SL(IBEG),    1, SIG, YUL, YLL)
        call CHROME (KNT, 1, BL(IBEG),    1, SIG, YUL, YLL)
        call CHROME (KNT, 1, BHSL(IBEG),  1, SIG, YUL, YLL)
        call CHROME (KNT, 1, XJNUL(IBEG), 1, SIG, YUL, YLL)
        call BELOWD (YLL, ONE, BOT)
        call ABOVED (YUL, ONE, TOP)
C     !EJECT
C----   Initialize plot image
        call KINIT    (IMAGE, ZL(IBEG), ZL(IEND), BOT, TOP, NV, NH,
     $                 NUMERO, GOOD)
        if(.not.GOOD) then
          call KRIGIA (ZL(IBEG), ZL(IEND), BOT, TOP, NV, NH)
        end if
C
C----   Enter points into image
        call SHRIMP   (ZL, N, IBEG, IEND, SL,    1, ALPHS(19), 1, SIG,
     $                 1, IMAGE)
        call SHRIMP   (ZL, N, IBEG, IEND, BL,    1, ALPHS( 2), 1, SIG,
     $                 1, IMAGE)
        call SHRIMP   (ZL, N, IBEG, IEND, BHSL,  1, ALPHS( 1), 1, SIG,
     $                 1, IMAGE)
        call SHRIMP   (ZL, N, IBEG, IEND, XJNUL, 1, ALPHS(10), 1, SIG,
     $                 1, IMAGE)
C
C----   Print plot header and image
        call ABJECT   (NO)
        write (NO,100) TIT,HEAD
  100   format(' ','Graph of log10s of S (=S), B (=B), Sabs (=A) and ',
     $             'J (=J) vs. ',A10,47X,A12)
        call LINER    (1, NO)
        call KPRINT   (IMAGE, NO)
C
C----   Now plot Tau vs. Z
        call MAY      (NO, ZL, TAU, N, IBEG, IEND, STAR, 0, 0)
C
      end if
C     !END
      call BYE ('DOODLE')
C
      return
      end
