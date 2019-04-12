      subroutine DAWDLE
     $(HEAD,N,Z,OPAC,SCAT,TAU,ZL,OL,SL,TL,NO)
C
C     Rudolf Loeser, 1973 May 23
C---- Produces an opacity plot.
C     !DASH
      save
C     !DASH
      real*8 BOT, OL, ONE, OPAC, SCAT, SIG, SL, TAU, TL, TOP, Z, ZAX,
     $       ZIN, ZL, dummy
      integer IBEG, IEND, KNT, N, NH, NO, NV
      logical OK
      character HEAD*12, NUMERO*1, TIT*10
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
      external LOGO, CHROME, BELOWD, ABOVED, ZED, KINIT, SHRIMP, LINER,
     $         ABJECT, KPRINT, KRIGIA, HI, BYE
C
C               SL(N), OL(N), TL(N), ZL(N), OPAC(N), SCAT(N), TAU(N),
      dimension SL(*), OL(*), TL(*), ZL(*), OPAC(*), SCAT(*), TAU(*),
C
C               Z(N)
     $          Z(*)
C
      data NV,NH /56, 117/
C
      call HI ('DAWDLE')
C     !BEG
      if(NO.gt.0) then
        SIG = ZL10SMA
C----   Compute logs
        call LOGO     (OPAC, N, 1, SIG, OL)
        call LOGO     (SCAT, N, 1, SIG, SL)
        call LOGO     (TAU,  N, 1, SIG, TL)
C----   Set up Z points
        IBEG = 0
        IEND = 0
        call ZED      (Z, N, dummy, 0, 0, IBEG, IEND, ZL, TIT,
     $                 'DAWDLE')
C----   Set up ordinate limits
        ZIN = +ZZLARGE
        ZAX = -ZZLARGE
        KNT =  IEND-(IBEG-1)
        call CHROME   (KNT, 1, OL(IBEG), 1, SIG, ZAX, ZIN)
        call CHROME   (KNT, 1, SL(IBEG), 1, SIG, ZAX, ZIN)
        call CHROME   (KNT, 1, TL(IBEG), 1, SIG, ZAX, ZIN)
        call BELOWD   (ZIN, ONE, BOT)
        call ABOVED   (ZAX, ONE, TOP)
C----   Initialize plot image
        call KINIT    (IMAGE, ZL(IBEG), ZL(IEND), BOT, TOP, NV, NH,
     $                 NUMERO, OK)
        if(.not.OK) then
          call KRIGIA (ZL(IBEG), ZL(IEND), BOT, TOP, NV, NH)
        end if
C     !EJECT
C----   Enter points into image
        call SHRIMP   (ZL, N, IBEG, IEND, OL, 1, ALPHS(11), 1, SIG, 1,
     $                 IMAGE)
        call SHRIMP   (ZL, N, IBEG, IEND, SL, 1, ALPHS(18), 1, SIG, 1,
     $                 IMAGE)
        call SHRIMP   (ZL, N, IBEG, IEND, TL, 1, ALPHS(20), 1, SIG, 1,
     $                 IMAGE)
C
C----   Write header and graph
        call ABJECT   (NO)
        write (NO,100) TIT,HEAD
  100   format(' ','Graph of log10s of Opacity (=K), Scattering Ratio',
     $             ' (=R), and Optical Depth (=T) vs. ',A10,24X,A12)
        call LINER    (1, NO)
        call KPRINT   (IMAGE, NO)
      end if
C     !END
      call BYE ('DAWDLE')
C
      return
      end
