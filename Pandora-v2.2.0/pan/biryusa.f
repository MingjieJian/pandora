      subroutine BIRYUSA
     $(N,K,IU,IL,DL,H,NO)
C
C     Rudolf Loeser, 1985 Jan 23
C---- Plots, for ALDAN.
C     (This is version 2 of BIRYUSA.)
C     !DASH
      save
C     !DASH
      real*8 BOT, DL, H, TOP
      integer II, IL, IMAX, IMIN, IU, K, N, NH, NK, NO, NV
      logical GOOD
      character NUMERO*1
C     !COM
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
      external MNMXD, TITAN, KRIGIA, YOLANDA, ZAGAL, INOPOS, BOUNDLO,
     $         KINIT, HI, BYE
C
C               H(N,K), DL(K)
      dimension H(*),   DL(*)
C
      dimension II(5)
C
      data NV,NH /51, 117/
C     !EJECT
C
      call HI ('BIRYUSA')
C     !BEG
      if(NO.gt.0) then
        NK = N*K
C----   Edit H values
        call BOUNDLO  (NK,H,ZZSMALL)
C----   Find graph limits
        call MNMXD    (H,1,NK,ZZSMALL,IMIN,IMAX)
        call TITAN    (H(IMIN),H(IMAX),BOT,TOP)
C----   Initialize graph image
        call KINIT    (IMAGE,DL(1),DL(K),BOT,TOP,NV,NH,NUMERO,GOOD)
        if(.not.GOOD) then
          call KRIGIA (DL(1),DL(K),BOT,TOP,NV,NH)
        end if
C----   Find indices of selected depths
        call YOLANDA  (N,II)
C----   Enter points in graph
        call ZAGAL    (II,ALPHS,N,K,DL,H,IMAGE)
C----   Print graph
        call INOPOS   (NO,IMAGE,IU,IL,II)
      end if
C     !END
      call BYE ('BIRYUSA')
C
      return
      end
