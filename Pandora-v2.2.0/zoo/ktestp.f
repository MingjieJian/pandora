      subroutine KTESTP
     $(W,J,I,INPLOT)
C     Rudolf Loeser, 1970 Dec 03
C---- Part of the K-type printer plot package;
C     see remarks in KTEXT.
C---- INPLOT will be set .true. if the plot position (J,I)
C     falls on or within the borders of the plot.
C     !DASH
      save
C     !DASH
      real*4 XOF, XSC, YOF, YSC, ZXH, ZXL, ZYH, ZYL
      integer I, J, KNH, KNT, KNV
      logical INPLOT
      character W*(*)
C     !COM
      common /KTYPE/ KNV,KNH,KNT, XSC,YSC,XOF,YOF, ZXL,ZXH,ZYL,ZYH
C
C     !BEG
      INPLOT = ((J.ge.1).and.(J.le.KNH)).and.
     $         ((I.ge.1).and.(I.le.KNV))
C     !END
C
      return
      end
