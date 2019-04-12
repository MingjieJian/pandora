      subroutine KPRINT
     $(W,NO)
C     Rudolf Loeser, 1970 Dec 03
C---- Part of the K-type printer plot package;
C     see remarks in KTEXT.
C     !DASH
      save
C     !DASH
      real*4 XOF, XSC, YOF, YSC, ZXH, ZXL, ZYH, ZYL
      integer I, JHB, JLB, KNH, KNT, KNV, NF, NFH, NFL, NO
      character BLANK*1, LINE*117, QXH*16, QXL*16, QZY*9, W*(*)
C     !COM
      common /KTYPE/ KNV,KNH,KNT, XSC,YSC,XOF,YOF, ZXL,ZXH,ZYL,ZYH
C     !DASH
      external ENCODER, KGIVE
C
      data BLANK /' '/
C
C     !BEG
      if(NO.gt.0) then
C
        call ENCODER (ZXL,QXL,16,6,1,NFL)
        call ENCODER (ZXH,QXH,16,6,1,NFH)
        JLB  = 17-NFL
        JHB  = 17-NFH
        LINE           = BLANK
        LINE(:NFL)     = QXL(JLB:)
        LINE(118-NFH:) = QXH(JHB:)
        write (NO,100) LINE
  100   format(' ',10X,A)
C
        call KGIVE   (W,1,LINE)
        call ENCODER (ZYH,QZY,9,6,1,NF)
        write (NO,101) QZY,LINE
  101   format(' ',A9,' ',A)
C
        do 102 I = 2,(KNV-1)
          call KGIVE (W,I,LINE)
          write (NO,100) LINE
  102   continue
C
        call KGIVE   (W,KNV,LINE)
        call ENCODER (ZYL,QZY,9,6,1,NF)
        write (NO,101) QZY,LINE
C
      end if
C     !END
C
      return
      end
