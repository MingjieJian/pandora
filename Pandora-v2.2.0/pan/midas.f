      subroutine MIDAS
     $(LU,IB,NAB,BANDL,BANDU,YL,YH,WAVCO,ARRCO,NCP,N,INDEX,KNT,COMP)
C
C     Rudolf Loeser, 1983 Jul 05
C---- Plots data for Band IB, for ZAB.
C     !DASH
      save
C     !DASH
      real*8 ARRCO, BANDL, BANDU, COMP, WAVCO, XL, XR, YH, YL
      integer I, IB, INDEX, J, JB, KNT, KUR, LINC, LU, N, NAB, NCP, NH,
     $        NV
      logical GOOD, MEMBER
      character NUMERO*1, STAR*1
C     !COM
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(38),NUMERO)
      equivalence (SYMBS(45),STAR  )
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
      external KINIT, KRIGIA, KPLOTC, ABJECT, LINER, KPRINT, KURASH,
     $         LINK, SET1, HI, BYE
C
C               INDEX(KNT), WAVCO(NCP), ARRCO(NCP,N), COMP(NCP),
      dimension INDEX(*),   WAVCO(*),   ARRCO(NCP,*), COMP(*),
C
C               BANDL(NAB), BANDU(NAB)
     $          BANDL(*),   BANDU(*)
C
      data NV,NH /56, 117/
C
      call HI ('MIDAS')
C     !BEG
C---- Initialize graph image
      XL = BANDL(IB)
      XR = BANDU(IB)
      call KINIT        (IMAGE, XL, XR, YL, YH, NV, NH, NUMERO, GOOD)
      if(.not.GOOD) then
        call KRIGIA     (XL, XR, YL, YH, NV, NH)
      end if
C
C---- Enter points
      call SET1         (COMP, NCP, ZZSMALL)
      do 102 KUR = 1,KNT
        J = INDEX(KUR)
        LINC = 1
        do 100 I = 1,NCP
          call KURASH   (NAB, BANDL, BANDU, WAVCO(I), MEMBER, JB)
          if(MEMBER.and.(JB.eq.IB)) then
            COMP(I) = log10(ARRCO(I,J))
            call LINK   (IMAGE, WAVCO(I), COMP(I), ALPHS(KUR), LINC)
          end if
  100   continue
        do 101 I = 1,NCP
          if(COMP(I).ne.ZZSMALL) then
            call KPLOTC (IMAGE, WAVCO(I), COMP(I), STAR)
          end if
  101   continue
  102 continue
C
C---- Print image
      call ABJECT       (LU)
      write (LU,103) IB
  103 format(' ','Plot of log(Composite Line Opacity) vs. wavelength, ',
     $           'Band #',I3,', at 5 selected depths.')
      call KPRINT       (IMAGE, LU)
      call LINER        (1, LU)
      write (LU,104) (INDEX(I),ALPHS(I),I=1,KNT)
  104 format(' ',5(6X,'Depth #',I3,' - ',A1))
C     !END
      call BYE ('MIDAS')
C
      return
      end
