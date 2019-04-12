      subroutine DULCIMR
     $(NO,WAVMI,WAVMA,INDEX,KNT,ARRK,ARRL,WL,TITLE,NW)
C
C     Rudolf Loeser, 1973 Aug 14
C---- Makes a plot of "Line" Opacity.
C     !DASH
      save
C     !DASH
      real*8 ARRK, ARRL, SIG, WAVMA, WAVMI, WL, XH, XL, YH, YL
      integer I, INDEX, J, KNT, NH, NO, NV, NW
      logical GOOD
      character NUMERO*1, PERIOD*1, TITLE*(*)
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
      external ABJECT, KRIGIA, KLINEC, HAUER, KPLOTC, KPRINT, OSSOLA,
     $         LOGO, KINIT, LINER, HI, BYE
C
C               ARRK(NW,N), WL(NW), INDEX(KNT), ARRL(NW,KNT)
      dimension ARRK(*),    WL(*),  INDEX(*),   ARRL(NW,*)
C
      data NV,NH /56, 117/
C
      call HI ('DULCIMR')
C     !BEG
      if(NO.gt.0) then
        SIG = ZL10SMA
C----   Select subset of array
        call OSSOLA       (ARRK, NW, INDEX, KNT, ARRL)
C----   Convert to logs
        call LOGO         (ARRL, (NW*KNT), 1, SIG, ARRL)
C----   Establish axes limits
        call HAUER        (KNT, NW, WL, ARRL, SIG, XL, XH, YL, YH)
C----   Initialize plot
        call KINIT        (IMAGE, XL, XH, YL, YH, NV, NH, NUMERO, GOOD)
        if(.not.GOOD) then
          call KRIGIA     (XL, XH, YL, YH, NV, NH)
        end if
        if(WAVMI.gt.XL) then
          call KLINEC     (IMAGE, WAVMI, YL, WAVMI, YH, PERIOD, 0)
        end if
        if(WAVMA.lt.XH) then
          call KLINEC     (IMAGE, WAVMA, YL, WAVMA, YH, PERIOD, 0)
        end if
C----   Enter data points
        do 101 J = 1,KNT
          do 100 I = 1,NW
            if(ARRL(I,J).ne.SIG) then
              call KPLOTC (IMAGE, WL(I), ARRL(I,J), ALPHS(J))
            end if
  100     continue
  101   continue
C----   Print image
        call ABJECT       (NO)
        write (NO,102) TITLE
  102   format(' ','Plot of log10(',A,' line opacity) vs. ',
     $             'wavelength, at 5 selected depths.')
        call KPRINT       (IMAGE, NO)
        call LINER        (1, NO)
        write (NO,103) (INDEX(I),ALPHS(I),I=1,KNT)
  103   format(' ',5(6X,'Depth #',I3,' - ',A1))
      end if
C     !END
      call BYE ('DULCIMR')
C
      return
      end
