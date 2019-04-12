      subroutine SPLOTC
     $(W,X,Y,SYM)
C     Rudolf Loeser
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
      external  SWHERE, KPLOTP
C
C     !BEG
      call SWHERE (W,X,Y,J,I)
      call KPLOTP (W,J,I,SYM)
C     !END
C
      return
      end
