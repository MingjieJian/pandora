      subroutine SCALDET
     $(N,SKALEC,SKALER,BOUND,DET)
C     Rudolf Loeser, 1984 Apr 04
C---- Re-scales the determinant computed from a scaled matrix.
C     !DASH
      save
C     !DASH
      real*8 BOUND, DAT, DET, DETLO, SKALEC, SKALER, SUM, TEN, UNDER,
     $       ZERO
      integer I, N
C     !DASH
      intrinsic sign, abs
C
      dimension SKALEC(N), SKALER(N)
C
      data      ZERO,TEN /0.D0, 1.D1/
C
C     !BEG
      if((DET.ne.ZERO).and.(DET.ne.BOUND)) then
        DETLO = log10(abs(DET))
        UNDER = -log10(BOUND)
C
        SUM = ZERO
        do 100 I = 1,N
          SUM = SUM+log10(SKALEC(I))+log10(SKALER(I))
  100   continue
C
        DETLO = DETLO-SUM
        if(DETLO.gt.UNDER) then
          DAT = TEN**DETLO
          DET = sign(DAT,DET)
        else
          DET = ZERO
        end if
      end if
C     !END
C
      return
      end
