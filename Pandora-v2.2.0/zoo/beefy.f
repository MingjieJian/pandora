      subroutine BEEFY
     $(IS,IE,DX,F,FP,DF)
C
C     Rudolf Loeser, 2003 Oct 28
C---- Updates the DF and FP tables, for SLATHER.
C     !DASH
      save
C     !DASH
      real*8 D, DF, DX, F, FP, ZERO
      integer I, IE, IS
C     !DASH
      external  SLURP
      intrinsic abs, max
C
      dimension DX(*), F(*), FP(*), DF(*)
C
      data ZERO /0.D0/
C
C     !BEG
      do 100 I = IS,IE
        call SLURP (DX, F, I, FP(I))
        DF(I) = (abs(F(I)-FP(I)))
        D     = max(abs(F(I)),abs(FP(I)))
        if(D.gt.ZERO) then
          DF(I) = DF(I)/D
        end if
  100 continue
C     !END
C
      return
      end
