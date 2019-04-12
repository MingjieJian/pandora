      subroutine HARP
     $(NO,J,DOJN,KSHL,TE,YW,N,RNU,MRP,XNU,XNUC)
C
C     Rudolf Loeser, 1973 Aug 14
C---- Makes a graph of Jnu vs. RNU, if needed.
C     !DASH
      save
C     !DASH
      real*8 BOT, ONE, RNU, TE, TOP, XH, XL, XNU, XNUC, YW
      integer II, IMAX, IMIN, J, MRP, N, NH, NMRP, NO, NV
      logical DOJN, GOOD, KSHL
      character LINE*117, NUMERO*1
C     !COM
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
      external MNMXD, TITAN, ABOVED, KINIT, YOLANDA, KRIGIA, BOUNDLO,
     $         ZAGAL, JUICE, HI, BYE
C
C               TE(N), YW(N,MRP), RNU(MRP), XNU(NSL), XNUC(NSL)
      dimension TE(*), YW(*),     RNU(*),   XNU(*),   XNUC(*)
C
      dimension II(5)
C
      data NV,NH /51, 117/
C
      call HI ('HARP')
C     !BEG
      if((MRP.gt.1).and.(NO.gt.0).and.DOJN) then
        NMRP = N*MRP
C----   Edit data
        call BOUNDLO  (NMRP, YW, ZZSMALL)
C----   Find graph limits
        call MNMXD    (YW, 1, NMRP, ZZSMALL, IMIN, IMAX)
        call TITAN    (YW(IMIN), YW(IMAX), BOT, TOP)
        XL = ONE
        call ABOVED   (RNU(MRP), ONE, XH)
C----   Initialize graph image
        call KINIT    (IMAGE, XL, XH, BOT, TOP, NV, NH, NUMERO, GOOD)
        if(.not.GOOD) then
          call KRIGIA (XL, XH, BOT, TOP, NV, NH)
        end if
C----   Find indices of selected depths
        call YOLANDA  (N, II)
C----   Enter points in graph
        call ZAGAL    (II, ALPHS, N, MRP, RNU, YW, IMAGE)
C----   Print graph
        call JUICE    (NO, IMAGE, J, KSHL, XH, XNU, XNUC, NH, II, LINE)
      end if
C     !END
      call BYE ('HARP')
C
      return
      end
