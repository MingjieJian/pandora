      subroutine HAUER
     $(KNT,KNW,WL,ARRL,SIG,XL,XH,YL,YH)
C
C     Rudolf Loeser, 1984 Sep 12
C---- Sets up axes limits, for DULCIMR.
C     (This is version 2 of HAUER.)
C     !DASH
      save
C     !DASH
      real*8 ARRL, BOT, ONE, SIG, TOP, WL, XH, XL, YH, YL
      integer KNT, KNW
C     !COM
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external CHROME, BELOWD, ABOVED, HI, BYE
C
C               WL(KNW), ARRL(KNW,KNT)
      dimension WL(*),   ARRL(*)
C
      call HI ('HAUER')
C     !BEG
      BOT = +ZZLARGE
      TOP = -ZZLARGE
      call CHROME (KNW,KNT,ARRL,1,SIG,TOP,BOT)
      call BELOWD (BOT,ONE,YL)
      call ABOVED (TOP,ONE,YH)
      XL = WL(1)
      XH = WL(KNW)
C     !END
      call BYE ('HAUER')
C
      return
      end
