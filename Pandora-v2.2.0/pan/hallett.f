      subroutine HALLETT
     $(NO,N,Z,ZT)
C
C     Rudolf Loeser, 2001 Nov 21
C---- Plots ZT, for EREBUS.
C     !DASH
      save
C     !DASH
      real*8 Z, ZAX, ZERO, ZIN, ZL, ZR, ZT
      integer N, NH, NO, NV
      logical GOOD
      character NUMERO*1, PERIOD*1, STAR*1
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(45),STAR  )
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
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !DASH
      external CHROME, SHRIMP, KRIGIA, KINIT, ABJECT, KLINEC, KPRINT,
     $         LINER, HI, BYE
C
C               Z(N), ZT(N)
      dimension Z(*), ZT(*)
C
      data NV,NH /55, 117/
C     !EJECT
C
      call HI ('HALLETT')
C     !BEG
C---- Set up ordinate
      ZIN = +ZZLARGE
      ZAX = -ZZLARGE
      call CHROME   (N,1,ZT,0,ZZSMALL,ZAX,ZIN)
C---- Set up abscissa
      ZL = Z(1)
      ZR = Z(N)
C---- Initialize plot image
      call KINIT    (IMAGE,ZL,ZR,ZIN,ZAX,NV,NH,NUMERO,GOOD)
      if(.not.GOOD) then
        call KRIGIA (ZL,ZR,ZIN,ZAX,NV,NH)
      end if
      call KLINEC   (IMAGE,ZL,ZERO,ZR,ZERO,PERIOD,0)
C---- Enter points into image
      call SHRIMP   (Z,N,1,N,ZT,1,STAR,1,ZZSMALL,2,IMAGE)
C---- Write header and graph
      call ABJECT   (NO)
      write (NO,100)
  100 format(' ','Plot of ZT vs. Z.')
      call LINER    (1,NO)
      call KPRINT   (IMAGE,NO)
C     !END
      call BYE ('HALLETT')
C
      return
      end
