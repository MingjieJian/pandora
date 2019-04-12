      subroutine SINIT
     $(W,XL,XH,YL,YH,NV,NH,SYM,GOOD)
C     Rudolf Loeser, 1970 Dec 03
C---- Part of the K-type printer plot package;
C     see remarks in KTEXT.
C     !DASH
      save
C     !DASH
      real*4 D, DX, DY, FUDGE, XH, XL, XOF, XSC, YH, YL, YOF, YSC, ZXH,
     $       ZXL, ZYH, ZYL
      integer KNH, KNT, KNV, NC, NH, NV
      logical GOOD
      character BLANK*1, SYM*1, W*(*)
C     !COM
      common /KTYPE/ KNV,KNH,KNT, XSC,YSC,XOF,YOF, ZXL,ZXH,ZYL,ZYH
C     !DASH
      external  KLINEP
      intrinsic min
C
      data BLANK /' '/
      data FUDGE /1.E-6/
C
C     !BEG
      GOOD = (XL.ne.XH).and.(YL.ne.YH).and.(NV.gt.0).and.(NH.gt.0)
      if(GOOD) then
C
        KNV = NV
        KNH = min(NH,117)
        KNT = 0
C
        D   = KNH-1
        DX  = XH-XL
        XSC = D/DX
        D   = KNV-1
        DY  = YH-YL
        YSC = D/DY
C
        XOF = XL-FUDGE*DX
        YOF = YL-FUDGE*DY
C
        ZXL = XL
        ZXH = XH
        ZYL = YL
        ZYH = YH
C
        NC = KNH*KNV
        W(:NC) = BLANK
C
        call KLINEP (W,  1,  1,KNH,  1,SYM,1)
        call KLINEP (W,  1,KNV,KNH,KNV,SYM,1)
        call KLINEP (W,  1,  1,  1,KNV,SYM,0)
        call KLINEP (W,KNH,  1,KNH,KNV,SYM,0)
C
      end if
C     !END
C
      return
      end
