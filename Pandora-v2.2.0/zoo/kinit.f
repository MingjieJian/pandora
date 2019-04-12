      subroutine KINIT
     $(W,XL,XH,YL,YH,NV,NH,SYM,GOOD)
C     Rudolf Loeser, 1979 Feb 24
C---- Part of the K-type printer plot package;
C     see remarks in KTEXT.
C     !DASH
      save
C     !DASH
      real*8 XH, XL, YH, YL
      real*4 XOF, XSC, XXH, XXL, YOF, YSC, YYH, YYL, ZXH, ZXL, ZYH, ZYL
      integer KNH, KNT, KNV, NH, NV
      logical GOOD
      character SYM*1, W*(*)
C     !COM
      common /KTYPE/ KNV,KNH,KNT, XSC,YSC,XOF,YOF, ZXL,ZXH,ZYL,ZYH
C     !DASH
      external  SINIT
C
C     !BEG
      XXL = XL
      XXH = XH
      YYL = YL
      YYH = YH
      call SINIT (W,XXL,XXH,YYL,YYH,NV,NH,SYM,GOOD)
C     !END
C
      return
      end
