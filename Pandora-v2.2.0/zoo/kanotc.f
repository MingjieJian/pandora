      subroutine KANOTC
     $(W,X,Y,STRING,L,KODE)
C     Rudolf Loeser, 1979 Feb 24
C---- Part of the K-type printer plot package;
C     see remarks in KTEXT.
C     !DASH
      save
C     !DASH
      real*8 X, Y
      real*4 XOF, XSC, YOF, YSC, ZXH, ZXL, ZYH, ZYL
      integer I, J, KNH, KNT, KNV, KODE, L
      character STRING*(*), W*(*)
C     !COM
      common /KTYPE/ KNV,KNH,KNT, XSC,YSC,XOF,YOF, ZXL,ZXH,ZYL,ZYH
C     !DASH
      external  KWHERE, KANOTP
C
C     !BEG
      call KWHERE (W,X,Y,J,I)
      call KANOTP (W,J,I,STRING,L,KODE)
C     !END
C
      return
      end
