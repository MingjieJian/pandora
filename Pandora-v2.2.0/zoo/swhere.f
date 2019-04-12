      subroutine SWHERE
     $(W,X,Y,J,I)
C     Rudolf Loeser, 1970 Dec 03
C---- Part of the K-type printer plot package;
C     see remarks in KTEXT.
C     !DASH
      save
C     !DASH
      real*4 X, XOF, XSC, Y, YOF, YSC, ZXH, ZXL, ZYH, ZYL
      integer I, J, KNH, KNT, KNV
      character W*(*)
C     !COM
      common /KTYPE/ KNV,KNH,KNT, XSC,YSC,XOF,YOF, ZXL,ZXH,ZYL,ZYH
C
C     !BEG
      J = (X-XOF)*XSC
      J = J+1
      I = (Y-YOF)*YSC
      I = KNV-I
C     !END
C
      return
      end
