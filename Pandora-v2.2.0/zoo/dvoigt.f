      subroutine DVOIGT
     $(X,A,VOIGT)
C     Rudolf Loeser, 1990 Dec 06
C---- Drives DRAYSON.
C     !DASH
      save
C     !DASH
      real*8 A, VOIGT, X
      real*4 V, XX, YY
C     !DASH
      external  DRAYSON
      intrinsic abs
C
C     !BEG
      XX = abs(X)
      YY = A
      call DRAYSON (XX,YY,V)
      VOIGT = V
C     !END
C
      return
      end
