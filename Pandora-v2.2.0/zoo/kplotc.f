      subroutine KPLOTC
     $(W,X,Y,SYM)
C     Rudolf Loeser, 1979 Feb 24
C---- Part of the K-type printer plot package;
C     see remarks in KTEXT.
C     !DASH
      save
C     !DASH
      real*8 X, Y
      real*4 XOF, XSC, YOF, YSC, ZXH, ZXL, ZYH, ZYL
      integer I, J, KNH, KNT, KNV
      character SYM*1, W*(*)
C     !COM
      common /KTYPE/ KNV,KNH,KNT, XSC,YSC,XOF,YOF, ZXL,ZXH,ZYL,ZYH
C     !DASH
      external  KWHERE, KPLOTP
C
C     !BEG
      call KWHERE (W,X,Y,J,I)
      call KPLOTP (W,J,I,SYM)
C     !END
C
      return
      end
