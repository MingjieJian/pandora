      subroutine BELOWD
     $(X,F,RES)
C     Rudolf Loeser, 1979 Apr 20
C---- RES will be the largest integral multiple of F
C     which is not larger than X.
C     F must be .gt. 0.
C     !DASH
      save
C     !DASH
      real*8 A, D, F, RES, X, Z, ZERO
C     !DASH
      external  ENTIERD
      intrinsic abs
C
      data ZERO /0.D0/
C
C     !BEG
      if(F.le.ZERO) then
        A = X
      else
        D = abs(F)
        call ENTIERD ((X/D),Z)
        A = D*Z
      end if
      RES = A
C     !END
C
      return
      end
