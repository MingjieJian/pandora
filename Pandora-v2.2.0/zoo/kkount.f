      subroutine KKOUNT
     $(W,KOUNT)
C     Rudolf Loeser, 1970 Dec 04
C---- Part of the K-type printer plot package;
C     see remarks in KTEXT.
C     !DASH
      save
C     !DASH
      real*4 XOF, XSC, YOF, YSC, ZXH, ZXL, ZYH, ZYL
      integer KNH, KNT, KNV, KOUNT
      character W*(*)
C     !COM
      common /KTYPE/ KNV,KNH,KNT, XSC,YSC,XOF,YOF, ZXL,ZXH,ZYL,ZYH
C
C     !BEG
      KOUNT = KNT
C     !END
C
      return
      end
