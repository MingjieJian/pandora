      subroutine KLINEC
     $(W,XA,YA,XB,YB,SYM,KODE)
C     Rudolf Loeser, 1979 FEB 24
C---- Part of the K-type printer plot package;
C     see remarks in KTEXT.
C     !DASH
      save
C     !DASH
      real*8 XA, XB, YA, YB
      real*4 XOF, XSC, YOF, YSC, ZXH, ZXL, ZYH, ZYL
      integer IA, IB, JA, JB, KNH, KNT, KNV, KODE
      character SYM*1, W*(*)
C     !COM
      common /KTYPE/ KNV,KNH,KNT, XSC,YSC,XOF,YOF, ZXL,ZXH,ZYL,ZYH
C     !DASH
      external KWHERE, KLINEP
C
C     !BEG
      call KWHERE (W,XA,YA,JA,IA)
      call KWHERE (W,XB,YB,JB,IB)
      call KLINEP (W,JA,IA,JB,IB,SYM,KODE)
C     !END
C
      return
      end
