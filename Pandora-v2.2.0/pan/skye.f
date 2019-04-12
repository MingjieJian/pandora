      subroutine SKYE
     $(NO,IU,IL,N,NP,P,PL,ZI,TE)
C
C     Rudolf Loeser, 1996 Mar 05
C---- Plots, for WAGRIN.
C     !DASH
      save
C     !DASH
      real*8 BOT, P, PL, SIG, TE, TOP, ZI, dummy
      integer IBEG, IEND, IL, IU, J, KOPT, N, NH, NO, NP, NV
      character PERIOD*1, TIT*10
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(42),PERIOD)
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
      external LOGO, RONAY, ZED, RHUM, SANDRAY, BARRA, SHIANT, SHRIMP,
     $         HI, BYE
C
C               P(N,NP), PL(N,NP), ZI(N), TE(N)
      dimension P(*),    PL(N,*),  ZI(*), TE(*)
C
      data NV,NH /55, 117/
C
      call HI ('SKYE')
C     !BEG
      if(NO.gt.0) then
        SIG = ZL10SMA
C----   Compute logs
        call LOGO     (P, (N*NP), 1, SIG, PL)
C----   Find starting index
        call RONAY    (PL, N, NP, SIG, IBEG)
C----   Establish abscissa
        IEND = N
        KOPT = 1
        call ZED      (dummy, N, dummy, 0, KOPT, IBEG, IEND, ZI, TIT,
     $                 'SKYE')
C----   Establish ordinate limits
        call RHUM     (N, NP, PL, SIG, BOT, TOP)
C----   Initialize plot image
        call SANDRAY  (IMAGE, ZI(IBEG), ZI(IEND), BOT, TOP, NV, NH)
C----   Enter lines
        do 100 J = 1,NP
          call SHRIMP (ZI, N, IBEG, IEND, PL(1,J), 1, PERIOD,   1,
     $                 SIG, 2, IMAGE)
  100   continue
C----   Enter points
        do 101 J = 2,NP
          call SHRIMP (ZI, N, IBEG, IEND, PL(1,J), 1, ALPHS(24), 1,
     $                 SIG, 1, IMAGE)
  101   continue
        call SHRIMP   (ZI, N, IBEG, IEND, PL(1,1), 1, ALPHS(15), 1,
     $                 SIG, 1, IMAGE)
C----   Enter temperature values
        call SHIANT   (IMAGE, NV, NH, TE, IBEG, IEND)
C----   Print plot
        call BARRA    (NO, IMAGE, TIT, IU, IL)
      end if
C     !END
      call BYE ('SKYE')
C
      return
      end
