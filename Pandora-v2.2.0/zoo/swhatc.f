      subroutine SWHATC
     $(W,X,Y,SYM)
C     Rudolf Loeser, 1970 Dec 03
C---- Part of the K-type printer plot package;
C     see remarks in KTEXT.
C     !DASH
      save
C     !DASH
      real*4 X, XOF, XSC, Y, YOF, YSC, ZXH, ZXL, ZYH, ZYL
      integer I, J, KNH, KNT, KNV
      character SYM*1, W*(*)
C     !COM
      common /KTYPE/ KNV,KNH,KNT, XSC,YSC,XOF,YOF, ZXL,ZXH,ZYL,ZYH
C     !DASH
      external  SWHERE, KWHATP
C
C     !BEG
      call SWHERE (W,X,Y,J,I)
      call KWHATP (W,J,I,SYM)
C     !END
C
      return
      end
