      subroutine SNOUT
     $(N,Z,ZL,XNE,XNEL,XNP,XNPL,ZHE,ZHEL,ZHV,ZHVL,LU)
C
C     Rudolf Loeser, 1984 May 11
C---- Plots electrons, and contributors.
C     !DASH
      save
C     !DASH
      real*8 BOT, ONE, SIG, TOP, XL, XNE, XNEL, XNP, XNPL, XR, YLL, YUL,
     $       Z, ZHE, ZHEL, ZHV, ZHVL, ZL, dummy
      integer IBEG, IEND, KNT, LU, N, NH, NV
      logical GOOD
      character NUMERO*1, STAR*1, TIT*10
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
      external ABJECT, CHROME, BELOWD, ABOVED, KRIGIA, SHRIMP, KPRINT,
     $         LINER, LOGO, KINIT, ZED, HI, BYE
C
C               XNEL(N), ZL(N), Z(N), XNE(N), XNP(N), XNPL(N), ZHVL(N),
      dimension XNEL(*), ZL(*), Z(*), XNE(*), XNP(*), XNPL(*), ZHVL(*),
C
C               ZHE(N), ZHEL(N), ZHV(N)
     $          ZHE(*), ZHEL(*), ZHV(*)
C
      data NV,NH /51, 117/
C
      call HI ('SNOUT')
C     !BEG
      if(LU.gt.0) then
        SIG = ZL10SMA
C----   Compute logs
        call LOGO     (XNE, N, 1, SIG, XNEL)
        call LOGO     (XNP, N, 1, SIG, XNPL)
        call LOGO     (ZHE, N, 1, SIG, ZHEL)
        call LOGO     (ZHV, N, 1, SIG, ZHVL)
C----   Establish Z points
        IBEG = 0
        IEND = 0
        call ZED      (Z, N, dummy, 0, 0, IBEG, IEND, ZL, TIT, 'SNOUT')
        XL = ZL(IBEG)
        XR = ZL(IEND)
C----   Find ordinate limits
        YUL = -ZZLARGE
        YLL = +ZZLARGE
        KNT =  IEND-(IBEG-1)
        call CHROME   (KNT, 1, XNEL(IBEG), 1, SIG, YUL, YLL)
        call BELOWD   (YLL, ONE, BOT)
        call ABOVED   (YUL, ONE, TOP)
C----   Initialize plot image
        call KINIT    (IMAGE, XL, XR, BOT, TOP, NV, NH, NUMERO, GOOD)
        if(.not.GOOD) then
          call KRIGIA (XL, XR, BOT, TOP, NV, NH)
        end if
C     !EJECT
C----   Enter data points into image
        call SHRIMP   (ZL, N, IBEG, IEND, ZHVL, 1, ALPHS(13), 1, SIG,
     $                 2, IMAGE)
        call SHRIMP   (ZL, N, IBEG, IEND, ZHEL, 1, STAR,      1, SIG,
     $                 2, IMAGE)
        call SHRIMP   (ZL, N, IBEG, IEND, XNPL, 1, ALPHS(16), 1, SIG,
     $                 2, IMAGE)
        call SHRIMP   (ZL, N, IBEG, IEND, XNEL, 1, ALPHS( 5), 1, SIG,
     $                 2, IMAGE)
C----   Print plot and legend
        call ABJECT   (LU)
        write (LU,100) TIT
  100   format(' ','Plot of log10(Number Density) vs. ',A10)
        call LINER    (1, LU)
        call KPRINT   (IMAGE, LU)
        call LINER    (1, LU)
        write (LU,101)
  101   format(' ','E = total electrons, P = protons, * = electrons ',
     $             'from Helium, M = electrons from elements ',
     $             'heavier than Helium.')
      end if
C     !END
      call BYE ('SNOUT')
C
      return
      end
