      subroutine KANOTP
     $(W,J,I,STRING,L,KODE)
C     Rudolf Loeser, 1970 Dec 04
C---- Part of the K-type printer plot package;
C     see remarks in KTEXT.
C     !DASH
      save
C     !DASH
      real*4 XOF, XSC, YOF, YSC, ZXH, ZXL, ZYH, ZYL
      integer I, II, J, JJ, K, KNH, KNT, KNV, KODE, L
      logical INPLOT
      character STRING*(*), W*(*)
C     !COM
      common /KTYPE/ KNV,KNH,KNT, XSC,YSC,XOF,YOF, ZXL,ZXH,ZYL,ZYH
C     !DASH
      external KTESTP, KPLOTP
C
C     !BEG
      call KTESTP     (W,J,I,INPLOT)
      if(INPLOT) then
        JJ = J
        II = I
        do 100 K = 1,L
          call KPLOTP (W,JJ,II,STRING(K:K))
          if(KODE.eq.1) then
            JJ = JJ+1
          else
            II = II+1
          end if
  100   continue
      end if
C     !END
C
      return
      end
