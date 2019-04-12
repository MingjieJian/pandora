      subroutine RAMA
     $(NO,L,NW,WAVLOG,XL,XR,LABEL,EMINT,KODA,EMINTA,LFB)
C
C     Rudolf Loeser, 1982 May 12
C---- Plots continuous intensity.
C     !DASH
      save
C     !DASH
      real*8 BOT, EMINT, EMINTA, ONE, TEN, TICK, TOP, WAVLOG, XL, XR,
     $       ZAX, ZIN
      integer KODA, L, LFB, NH, NO, NV, NW
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
      external  CARNEL, KPRINT, KRIGIA, MONKEY, OSUNA, TUMBLE, ABJECT,
     $          LINER, TITAN, KINIT, HI, BYE
      intrinsic max
C
C               WAVLOG(Nmkuse), EMINT(Nmkuse,L), EMINTA(Nmkuse)
      dimension WAVLOG(*),      EMINT(*),        EMINTA(*)
C
      data NV,NH /55,117/
C     !EJECT
C
      call HI ('RAMA')
C     !BEG
C---- Edit values, and find extrema
      call CARNEL   (EMINT,(NW*L),ZAX,ZIN)
C---- Set up ordinate limits
      call TITAN    (ZIN,ZAX,BOT,TOP)
C
C---- Initialize plot image
      call KINIT    (IMAGE,XL,XR,BOT,TOP,NV,NH,NUMERO,GOOD)
      if(.not.GOOD) then
        call KRIGIA (XL,XR,BOT,TOP,NV,NH)
      end if
      call MONKEY   (IMAGE,XL ,XR ,ONE ,BOT,TOP,PERIOD,1)
      TICK = max(ONE,((TOP-BOT)/TEN))
      call MONKEY   (IMAGE,BOT,TOP,TICK,XL ,XR ,PERIOD,2)
C
C---- Enter data
      call OSUNA    (IMAGE,WAVLOG,EMINT,1,L,NW,KODA,EMINTA,LFB)
C
C---- Print header and graph
      LINE = ' Log(Emergent Intensity) vs. '//LABEL
      call TUMBLE   (LFB,LINE(118:127))
      call ABJECT   (NO)
      write (NO,100) LINE
  100 format(' ',A)
      call LINER    (1,NO)
      call KPRINT   (IMAGE,NO)
C     !END
      call BYE ('RAMA')
C
      return
      end
