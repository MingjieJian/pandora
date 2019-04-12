      subroutine ADARE
     $(NO,N,VEC,TE,DTE,SDTE)
C
C     Rudolf Loeser, 2001 Nov 23
C---- Plots T and DTE, for EREBUS.
C     !DASH
      save
C     !DASH
      real*8 BOT, DTE, SDTE, TE, TEN, TOP, VEC, XL, XR
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
      external THATELL, CHEMOR, INDARRD, SHRIMP, KRIGIA, ABJECT, LINER,
     $         KINIT, KPRINT, MONKEY, HI, BYE
C
C               VEC(N), TE(N), DTE(N), SDTE(N)
      dimension VEC(*), TE(*), DTE(*), SDTE(*)
C
      data NV,NH /55, 117/
C     !EJECT
C
      call HI ('ADARE')
C     !BEG
C---- Set up ordinate
      call CHEMOR   (N,TE,BOT,TOP)
C---- Set up scaled DTE
      call THATELL  (N,DTE,BOT,TOP,SDTE)
C---- Set up abscissa
      call INDARRD  (VEC,1,1,N)
      XL = VEC(1)
      XR = VEC(N)
C---- Initialize plot image
      call KINIT    (IMAGE,XL,XR,BOT,TOP,NV,NH,NUMERO,GOOD)
      if(.not.GOOD) then
        call KRIGIA (XL,XR,BOT,TOP,NV,NH)
      end if
      call MONKEY   (IMAGE,XL,XR,TEN,BOT,TOP,PERIOD,1)
C---- Enter points into image
      call SHRIMP   (VEC,N,1,N,TE  ,1,ALPHS(20),1,ZZSMALL,2,IMAGE)
      call SHRIMP   (VEC,N,1,N,SDTE,1,ALPHS( 4),1,ZZSMALL,2,IMAGE)
C---- Write header and graph
      call ABJECT   (NO)
      write (NO,100)
  100 format(' ','Plot of TE (=T) and DTE (scaled and shifted to fit ',
     $           'the graph) (=D) vs. depth index.')
      call LINER    (1,NO)
      call KPRINT   (IMAGE,NO)
C     !END
      call BYE ('ADARE')
C
      return
      end
