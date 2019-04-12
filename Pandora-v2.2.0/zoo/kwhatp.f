      subroutine KWHATP
     $(W,J,I,SYM)
C     Rudolf Loeser, 1970 Dec 03
C---- Part of the K-type printer plot package;
C     see remarks in KTEXT.
C     !DASH
      save
C     !DASH
      real*4 XOF, XSC, YOF, YSC, ZXH, ZXL, ZYH, ZYL
      integer I, IJ, J, KNH, KNT, KNV
      logical INPLOT
      character BLANK*1, SYM*1, W*(*)
C     !COM
      common /KTYPE/ KNV,KNH,KNT, XSC,YSC,XOF,YOF, ZXL,ZXH,ZYL,ZYH
C     !DASH
      external KTESTP
C
      data BLANK /' '/
C
C     !BEG
      call KTESTP (W,J,I,INPLOT)
      if(INPLOT) then
        IJ  = J+KNH*(I-1)
        SYM = W(IJ:IJ)
      else
        SYM = BLANK
      end if
C     !END
C
      return
      end
