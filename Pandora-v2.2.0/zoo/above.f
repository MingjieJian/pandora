      subroutine ABOVE
     $(X,F,RES)
C     Rudolf Loeser, 1979 Apr 20
C---- RES will be the smallest integral multiple of F
C     which is not smaller than X.
C     F must be .gt. 0.
C     !DASH
      save
C     !DASH
      real*4 A, F, Q, RES, X, Z, ZERO
C     !DASH
      external  ENTIER
      intrinsic abs
C
      data      ZERO /0.E0/
C
C     !BEG
      if(F.le.ZERO) then
        A = X
      else
        Q = abs(F)
        call ENTIER ((-X/Q),Z)
        A = -Q*Z
      end if
      RES = A
C     !END
C
      return
      end
