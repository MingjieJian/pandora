      subroutine MELANIE
     $(N,Z,XNE,HND,XNHM,HN1,ZL,XNEL,HNDL,XNHML,HN1L,NO)
C
C     Rudolf Loeser, 1973 May 11
C---- Plots number densities, for OSIRIS.
C     !DASH
      save
C     !DASH
      real*8 HN1, HN1L, HND, HNDL, SIG, XL, XNE, XNEL, XNHM, XNHML, XR,
     $       Z, ZAX, ZIN, ZL, dummy
      integer IBEG, IEND, N, NH, NO, NV
      logical GOOD
      character MINUS*1, NUMERO*1, TIT*10
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(38),NUMERO)
      equivalence (SYMBS(40),MINUS )
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
      external LOGO, ZED, KPRINT, KINIT, SHRIMP, ABJECT, KRIGIA, LINER,
     $         RUIZ, HI, BYE
C
C               ZL(N), Z(N), XNE(N), HND(N), XNHM(N), HN1(N), XNHML(N),
      dimension ZL(*), Z(*), XNE(*), HND(*), XNHM(*), HN1(*), XNHML(*),
C
C               XNEL(N), HNDL(N), HN1L(N)
     $          XNEL(*), HNDL(*), HN1L(*)
C
      data NV,NH /56, 117/
C
      call HI ('MELANIE')
C     !BEG
      if(NO.gt.0) then
        SIG = ZL10SMA
C----   Compute logs
        call LOGO     (XNE,  N, 1, SIG, XNEL )
        call LOGO     (HND,  N, 1, SIG, HNDL )
        call LOGO     (XNHM, N, 1, SIG, XNHML)
        call LOGO     (HN1,  N, 1, SIG, HN1L )
C----   Establish Z points
        IBEG = 0
        IEND = 0
        call ZED      (Z, N, dummy, 0, 0, IBEG, IEND, ZL, TIT,
     $                 'MELANIE')
        XL = ZL(IBEG)
        XR = ZL(IEND)
C----   Set up ordinate
        call RUIZ     (XNEL, HNDL, XNHML, HN1L, SIG, IBEG, IEND,
     $                 ZIN, ZAX)
C----   Initialize plot image
        call KINIT    (IMAGE, XL, XR, ZIN, ZAX, NV, NH, NUMERO, GOOD)
        if(.not.GOOD) then
          call KRIGIA (XL, XR, ZIN, ZAX, NV, NH)
        end if
C----   Enter points into image
        call SHRIMP   (ZL, N, IBEG, IEND, XNEL,  1, ALPHS(5), 1, SIG,
     $                 2, IMAGE)
        call SHRIMP   (ZL, N, IBEG, IEND, HNDL,  1, ALPHS(8), 1, SIG,
     $                 2, IMAGE)
        call SHRIMP   (ZL, N, IBEG, IEND, XNHML, 1, MINUS,    1, SIG,
     $                 2, IMAGE)
        call SHRIMP   (ZL, N, IBEG, IEND, HN1L,  1, NUMBS(2), 1, SIG,
     $                 2, IMAGE)
C----   Write header and graph
        call ABJECT   (NO)
        write (NO,100) TIT
  100   format(' ','Plot of log10s of NE (=E), NH (=H), HN1 (=1) ',
     $             'and NHM (=-) vs. ',A10)
        call LINER    (1, NO)
        call KPRINT   (IMAGE, NO)
      end if
C     !END
      call BYE ('MELANIE')
C
      return
      end
