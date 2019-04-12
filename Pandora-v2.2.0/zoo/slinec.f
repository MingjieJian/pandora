      subroutine SLINEC
     $(W,XA,YA,XB,YB,SYM,KODE)
C     Rudolf Loeser, 1970 Dec 04
C---- Part of the K-type printer plot package;
C     see remarks in KTEXT.
C     !DASH
      save
C     !DASH
      real*4 XA, XB, XOF, XSC, YA, YB, YOF, YSC, ZXH, ZXL, ZYH, ZYL
      integer IA, IB, JA, JB, KNH, KNT, KNV, KODE
      character SYM*1, W*(*)
C     !COM
      common /KTYPE/ KNV,KNH,KNT, XSC,YSC,XOF,YOF, ZXL,ZXH,ZYL,ZYH
C     !DASH
      external SWHERE, KLINEP
C
C     !BEG
      call SWHERE (W,XA,YA,JA,IA)
      call SWHERE (W,XB,YB,JB,IB)
      call KLINEP (W,JA,IA,JB,IB,SYM,KODE)
C     !END
C
      return
      end
