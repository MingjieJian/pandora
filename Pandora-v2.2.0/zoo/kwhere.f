      subroutine KWHERE
     $(W,X,Y,J,I)
C     Rudolf Loeser, 1979 Feb 24
C---- Part of the K-type printer plot package;
C     see remarks in KTEXT.
C     !DASH
      save
C     !DASH
      real*8 X, Y
      real*4 XOF, XSC, XX, YOF, YSC, YY, ZXH, ZXL, ZYH, ZYL
      integer I, J, KNH, KNT, KNV
      character W*(*)
C     !COM
      common /KTYPE/ KNV,KNH,KNT, XSC,YSC,XOF,YOF, ZXL,ZXH,ZYL,ZYH
C     !DASH
      external  SWHERE
C
C     !BEG
      XX = X
      YY = Y
      call SWHERE (W,XX,YY,J,I)
C     !END
C
      return
      end
