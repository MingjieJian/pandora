      subroutine CALICUT
     $(LU,N,M,X,PND,PMAX)
C
C     Rudolf Loeser, 2007 Mar 28
C---- Plots normalized number densities.
C     !DASH
      save
C     !DASH
      real*8 BOT, ONE, PMAX, PMAXL, PND, SIG, TEN, TOP, X, XL, XR, ZERO
      integer IMAX, IMIN, LU, M, N, NH, NV
      logical GOOD
      character NUMERO*1, STAR*1
C     !COM  or  !DASH
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT(11),TEN   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(45),STAR  )
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
      external ABOVED, INDARRD, KINIT, KRIGIA, SHRIMP, ABJECT, KPRINT,
     $         LOGO, HI, BYE
C
C               PND(N,M), X(N)
      dimension PND(N,*), X(*)
C
      data NV,NH /55, 117/
C
      call HI ('CALICUT')
C     !BEG
C---- Take logs of normalized number densities
      SIG = ZL10SMA
      call LOGO     (PND, (N*M), 1, SIG, PND)
      PMAXL = log10(PMAX)
C---- Set up top abscissa limit
      call ABOVED   (PMAXL, ONE, TOP)
C---- Set up ordinate
      call INDARRD  (X, 1, 1, N)
C---- Set up plot
      XL  = X(1)
      XR  = X(N)
      BOT = TOP-TEN
      call KINIT    (IMAGE, XL, XR, BOT, TOP, NV, NH, NUMERO, GOOD)
      if(.not.GOOD) then
        call KRIGIA (XL, XR, BOT, TOP, NV, NH)
      end if
C---- Enter data
      call SHRIMP   (X, N, 1, N, PND(1,1), (M-1), ALPHS, 26, SIG, 2,
     $               IMAGE)
      call SHRIMP   (X, N, 1, N, PND(1,M), 1,     STAR,   1, SIG, 2,
     $               IMAGE)
C---- Print
      call ABJECT   (LU)
      write (LU,100)
  100 format(' ','Log10(Number Densities / HND) vs. Z-index.')
      call KPRINT   (IMAGE, LU)
C     !END
      call BYE ('CALICUT')
C
      return
      end
