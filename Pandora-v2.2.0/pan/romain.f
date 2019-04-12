      subroutine ROMAIN
     $(XND,N,NL,LU,X,RND,ZN,NN,IPNT,IWRK)
C
C     Rudolf Loeser, 1999 Nov 05
C---- Generates a plot of level-population-ranks.
C     (This is version 2 of ROMAIN.)
C     !DASH
      save
C     !DASH
      real*8 BOT, RND, TEN, TOP, X, XL, XND, XR, ZERO, ZN
      integer IFLG, IPNT, IWRK, LU, ML, N, NH, NL, NN, NV
      logical GOOD
      character NUMERO*1, PERIOD*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(38),NUMERO)
      equivalence (SYMBS(42),PERIOD)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(11),TEN   )
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
      external  ABJECT, DOGWOOD, KINIT, KRIGIA, MONKEY, SHRIMP, KPRINT,
     $          INDARRD, HI, BYE
      intrinsic min
C
C               XND(N,NL), X(N), RND(N,NL), IPNT(NL), IWRK(NL), ZN(NL),
      dimension XND(*),    X(*), RND(*),    IPNT(*),  IWRK(*),  ZN(*),
C
C               NN(NL)
     $          NN(*)
C
      data NV,NH /55, 117/
C
      call HI ('ROMAIN')
C     !BEG
      call DOGWOOD    (XND, N, NL, RND, ZN, NN, IPNT, IWRK, IFLG)
C
      if(IFLG.gt.0) then
        call INDARRD  (X, 1, 1, N)
        ML  = min(NL,26)
        XL  = X(1)
        XR  = X(N)
        BOT = ZERO
        TOP = ML+1
        call KINIT    (IMAGE, XL, XR, BOT, TOP, NV, NH, NUMERO, GOOD)
        if(.not.GOOD) then
          call KRIGIA (XL, XR, BOT, TOP, NV, NH)
        end if
        call MONKEY   (IMAGE, XL, XR, TEN, BOT, TOP, PERIOD, 1)
C
        call SHRIMP   (X, N, 1, N, RND, ML, ALPHS, ML, -TEN, 2, IMAGE)
      end if
C
      call ABJECT     (LU)
      write (LU,100)
  100 format(' ','Rank of level populations vs. Z-index.')
      if(IFLG.gt.0) then
        call KPRINT   (IMAGE, LU)
C
      else
        write (LU,101)
  101   format(' ','Note: unable to set up this graph -- skipped.')
      end if
C     !END
      call BYE ('ROMAIN')
C
      return
      end
