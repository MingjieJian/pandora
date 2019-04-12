      subroutine SANOTC
     $(W,X,Y,STRING,L,KODE)
C     Rudolf Loeser, 1970 Dec 04
C---- Part of the K-type printer plot package;
C     see remarks in KTEXT.
C     !DASH
      save
C     !DASH
      real*4 X, XOF, XSC, Y, YOF, YSC, ZXH, ZXL, ZYH, ZYL
      integer I, J, KNH, KNT, KNV, KODE, L
      character STRING*(*), W*(*)
C     !COM
      common /KTYPE/ KNV,KNH,KNT, XSC,YSC,XOF,YOF, ZXL,ZXH,ZYL,ZYH
C     !DASH
      external SWHERE, KANOTP
C
C     !BEG
      call SWHERE (W,X,Y,J,I)
      call KANOTP (W,J,I,STRING,L,KODE)
C     !END
C
      return
      end
