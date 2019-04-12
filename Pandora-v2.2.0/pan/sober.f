      subroutine SOBER
     $(N,TAU,Z,US,S,B,IU,IL,NO,SL,BL,ZL,USL,ENHANCE)
C
C     Rudolf Loeser, 1968 Feb 20
C---- Plots log(S), log(B) and log(Tau) vs. Z.
C     !DASH
      save
C     !DASH
      real*8 B, BL, BOT, ONE, S, SIG, SL, TAU, TOP, US, USL, XL, XR,
     $       YLL, YUL, Z, ZL
      integer IBEG, IEND, IL, IU, KNT, N, NH, NO, NV
      logical ENHANCE, GOOD
      character LINE*128, NUMERO*1, STAR*1, TIT*10
C     !COM
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
C     !DASH
C     !EJECT
      external LINER, ZED, BELOWD, ABOVED, MAY, SHRIMP, ABJECT, KRIGIA,
     $         CHROME, LOGO, KPRINT, KINIT, HI, BYE
C
C               SL(N), BL(N), ZL(N), TAU(N), Z(N), S(N), US(N), USL(N),
      dimension SL(*), BL(*), ZL(*), TAU(*), Z(*), S(*), US(*), USL(*),
C
C               B(N)
     $          B(*)
C
      dimension LINE(2)
C
      data LINE(2) /' The symbol * is used for values of S modified by t
     $he R**2 enhancement factor.'/
      data NV,NH /51, 117/
C
      call HI ('SOBER')
C     !BEG
      if(NO.gt.0) then
        SIG = ZL10SMA
C----   Compute logs of S and B
        call LOGO     (S,  N, 1, SIG, SL )
        call LOGO     (B,  N, 1, SIG, BL )
        if(ENHANCE) then
          call LOGO   (US, N, 1, SIG, USL)
        end if
C----   Establish Z points
        IBEG = 0
        IEND = 0
        call ZED      (Z, N, TAU, N, 2, IBEG, IEND, ZL, TIT, 'SOBER')
        XL = ZL(IBEG)
        XR = ZL(IEND)
        write (LINE(1),100) TIT,IU,IL
  100   format(' ','Graph of log10(S) and log10(B) vs. ',A10,
     $             ' for transition (',I2,'/',I2,').')
C----   Find limits of ordinate
        YLL = +ZZLARGE
        YUL = -ZZLARGE
        KNT = IEND-(IBEG-1)
        call CHROME   (KNT, 1, SL(IBEG), 1, SIG, YUL, YLL)
        call CHROME   (KNT, 1, BL(IBEG), 1, SIG, YUL, YLL)
        call BELOWD   (YLL, ONE, BOT)
        call ABOVED   (YUL, ONE, TOP)
C----   Initialize plot image
        call KINIT    (IMAGE, XL, XR, BOT, TOP, NV, NH, NUMERO, GOOD)
        if(.not.GOOD) then
          call KRIGIA (XL, XR, BOT, TOP, NV, NH)
        end if
C     !EJECT
C----   Enter points into image
        if(ENHANCE) then
          call SHRIMP (ZL, N, IBEG, IEND, USL, 1, STAR,      1, SIG, 2,
     $                 IMAGE)
        end if
        call SHRIMP   (ZL, N, IBEG, IEND, SL,  1, ALPHS(19), 1, SIG, 2,
     $                 IMAGE)
        call SHRIMP   (ZL, N, IBEG, IEND, BL,  1, ALPHS( 2), 1, SIG, 2,
     $                 IMAGE)
C----   Write graph header and image
        call ABJECT   (NO)
        write (NO,101) LINE(1)
  101   format(A128)
        if(ENHANCE) then
          write (NO,101) LINE(2)
        end if
        call LINER    (1, NO)
        call KPRINT   (IMAGE, NO)
C----   Now plot Tau vs. Z
        call MAY      (NO, ZL, TAU, N, IBEG, IEND, STAR, IU, IL)
      end if
C     !END
      call BYE ('SOBER')
C
      return
      end
