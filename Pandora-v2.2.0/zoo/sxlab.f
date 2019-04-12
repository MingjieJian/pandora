      subroutine SXLAB
     $(NO,XL,XH)
C
C     Rudolf Loeser, 2006 May 09
C---- Part of the K-type printer plot package.
C     Prints x-axis limits for the plot.
C     !DASH
      save
C     !DASH
      real*4 XH, XL, XOF, XSC, YOF, YSC, ZXH, ZXL, ZYH, ZYL
      integer JHB, JLB, KNH, KNT, KNV, MST, NFH, NFL, NO
      character BLANK*1, LINE*117, QXH*16, QXL*16
C     !COM
      common /KTYPE/ KNV,KNH,KNT, XSC,YSC,XOF,YOF, ZXL,ZXH,ZYL,ZYH
C     !DASH
      external ENCODER
C
      data BLANK /' '/
C
C     !BEG
      if(NO.gt.0) then
        call ENCODER (XL, QXL, 16, 6, 1, NFL)
        call ENCODER (XH, QXH, 16, 6, 1, NFH)
C
        JLB  = 17-NFL
        JHB  = 17-NFH
        MST  = (KNH+1)-NFH
C
        LINE       = BLANK
        LINE(:NFL) = QXL(JLB:)
        LINE(MST:) = QXH(JHB:)
C
        write (NO,100) LINE
  100   format(' ',10X,A)
      end if
C     !END
C
      return
      end
