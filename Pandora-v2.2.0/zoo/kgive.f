      subroutine KGIVE
     $(W,N,LINE)
C     Rudolf Loeser, 1970 Dec 03
C---- Part of the K-type printer plot package;
C     see remarks in KTEXT.
C     !DASH
      save
C     !DASH
      real*4 XOF, XSC, YOF, YSC, ZXH, ZXL, ZYH, ZYL
      integer KE, KK, KNH, KNT, KNV, KS, N
      character BLANK*1, LINE*(*), W*(*)
C     !COM
      common /KTYPE/ KNV,KNH,KNT, XSC,YSC,XOF,YOF, ZXL,ZXH,ZYL,ZYH
C     !DASH
      data BLANK /' '/
C
C     !BEG
      if((N.ge.1).and.(N.le.KNV)) then
        KK = KNH*(N-1)
        KS = KK+1
        KE = KK+KNH
        LINE(:KNH) = W(KS:KE)
      else
        LINE(:KNH) = BLANK
      end if
C     !END
C
      return
      end
