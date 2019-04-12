      subroutine SITA
     $(NO,L,NW,WAVLOG,XL,XR,LABEL,BRIGHT,KODA,BRIGHTA,LFB)
C
C     Rudolf Loeser, 1982 May 12
C---- Plots brightness temperature.
C     (This is version 2 of SITA.)
C     !DASH
      save
C     !DASH
      real*8 BAX, BIN, BL, BOT, BRIGHT, BRIGHTA, ONE, TEN, THSND, TICK,
     $       TINT, TOP, TOPPY, WAVLOG, XL, XR
      integer KODA, L, LFB, MIOW, NH, NO, NV, NW
      logical GOOD
      character LABEL*15, LINE*127, NUMERO*1, PERIOD*1
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
      equivalence (SYMBS(38),NUMERO)
      equivalence (SYMBS(42),PERIOD)
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
C     !EJECT
      external  CARNEL, KPRINT, BELOWD, ABOVED, KRIGIA, MONKEY, TUMBLE,
     $          OSUNA, ABJECT, LINER, KINIT, HI, BYE
      intrinsic max, min
C
C               WAVLOG(Nmkuse), BRIGHT(Nmkuse,L), BRIGHTA(Nmkuse)
      dimension WAVLOG(*),      BRIGHT(*),        BRIGHTA(*)
C
      data THSND,TOPPY /1.D3, 1.D12/
      data NV,NH /55, 117/
C
      call HI ('SITA')
C     !BEG
C---- Edit values, and find extrema
      call CARNEL   (BRIGHT, (NW*L), BAX, BIN)
C---- Set up ordinate limits
      BL   = log10(BAX-BIN)
      MIOW = BL
      TINT = TEN**MIOW
      call BELOWD   (BIN, TINT, BOT)
      call ABOVED   (BAX, TINT, TOP)
      TOP  = min(TOP,TOPPY)
      TICK = max(THSND,(TOP/TEN))
C
C---- Initialize plot image
      call KINIT    (IMAGE, XL, XR, BOT, TOP, NV, NH, NUMERO, GOOD)
      if(.not.GOOD) then
        call KRIGIA (XL, XR, BOT, TOP, NV, NH)
      end if
      call MONKEY   (IMAGE, XL,  XR,  ONE,  BOT, TOP, PERIOD, 1)
      call MONKEY   (IMAGE, BOT, TOP, TICK, XL,  XR,  PERIOD, 2)
C
C---- Enter data
      call OSUNA    (IMAGE, WAVLOG, BRIGHT, 0, L, NW, KODA, BRIGHTA,
     $               LFB)
C
C---- Print header and graph
      call ABJECT   (NO)
      LINE = ' Brightness Temperature vs. '//LABEL
      call TUMBLE   (LFB, LINE(118:127))
      write (NO,100) LINE
  100 format(' ',A)
      call LINER    (1, NO)
      call KPRINT   (IMAGE, NO)
C     !END
      call BYE ('SITA')
C
      return
      end
