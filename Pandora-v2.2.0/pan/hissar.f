      subroutine HISSAR
     $(W,K,WVL,DL,YHZ,XHZ,EW,YIN)
C
C     Rudolf Loeser, 1983 Jul 28
C---- Computes integrated ray intensity.
C     !DASH
      save
C     !DASH
      real*8 DL, EW, W, WVL, XHZ, YHZ, YIN
      integer I, K
C     !DASH
      external CASPIA, FIMOOR, ONYX, HI, BYE
C
      dimension W(*)
C
C               EW(K), YHZ(K), YIN(K), XHZ(K), DL(K)
      dimension EW(*), YHZ(*), YIN(*), XHZ(*), DL(*)
C
      call HI ('HISSAR')
C     !BEG
      call CASPIA (1,K,YHZ,XHZ)
      call FIMOOR (W,K,DL,XHZ,EW)
      do 100 I = 1,K
        call ONYX (WVL,DL(I),EW(I),YHZ(I),YIN(I))
  100 continue
C     !END
      call BYE ('HISSAR')
C
      return
      end
