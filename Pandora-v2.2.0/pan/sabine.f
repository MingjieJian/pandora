      subroutine SABINE
     $(NO,N,VEC,XNE,HND,XNL,HNL)
C
C     Rudolf Loeser, 2001 Nov 21
C---- Plots NE and NH, for EREBUS.
C     !DASH
      save
C     !DASH
      real*8 BOT, FACB, FACT, HND, HNL, ONE, SIG, TEN, TOP, VEC, XL,
     $       XNE, XNL, XR, ZAX, ZIN
      integer N, NH, NO, NV
      logical GOOD
      character NUMERO*1, PERIOD*1
C     !COM
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
      equivalence (SYMBS(42),PERIOD)
      equivalence (SYMBS(38),NUMERO)
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
      external MONKEY, CHROME, BELOWD, ABOVED, INDARRD, SHRIMP, KRIGIA,
     $         KINIT, LOGO, ABJECT, LINER, KPRINT, HI, BYE
C
C               XNE(N), HND(N), VEC(N), XNL(N), HNL(N)
      dimension XNE(*), HND(*), VEC(*), XNL(*), HNL(*)
C
      data NV,NH /55, 117/
      data FACB, FACT /9.9D-1, 1.01D0/
C
      call HI ('SABINE')
C     !BEG
      SIG = ZL10SMA
C---- Compute logs
      call LOGO     (XNE, N, 1, SIG, XNL)
      call LOGO     (HND, N, 1, SIG, HNL)
C---- Set up ordinate
      ZIN = +ZZLARGE
      ZAX = -ZZLARGE
      call CHROME   (N, 1, XNL, 1, SIG, ZAX, ZIN)
      call CHROME   (N, 1, HNL, 1, SIG, ZAX, ZIN)
      call BELOWD   (ZIN, ONE, BOT)
      call ABOVED   (ZAX, ONE, TOP)
      if(BOT.eq.TOP) then
        BOT = FACB*BOT
        TOP = FACT*TOP
      end if
C---- Set up abscissa
      call INDARRD  (VEC, 1, 1, N)
      XL = VEC(1)
      XR = VEC(N)
C---- Initialize plot image
      call KINIT    (IMAGE, XL, XR, BOT, TOP, NV, NH, NUMERO, GOOD)
      if(.not.GOOD) then
        call KRIGIA (XL, XR, BOT, TOP, NV, NH)
      end if
      call MONKEY   (IMAGE, XL, XR, TEN, BOT, TOP, PERIOD, 1)
C---- Enter points into image
      call SHRIMP   (VEC, N, 1, N, XNL, 1, ALPHS(5), 1, SIG, 2, IMAGE)
      call SHRIMP   (VEC, N, 1, N, HNL, 1, ALPHS(8), 1, SIG, 2, IMAGE)
C---- Write header and graph
      call ABJECT   (NO)
      write (NO,100)
  100 format(' ','Plot of log10 of NE (=E) and NH (=H) vs. depth ',
     $           'index.')
      call LINER    (1, NO)
      call KPRINT   (IMAGE, NO)
C     !END
      call BYE ('SABINE')
C
      return
      end
