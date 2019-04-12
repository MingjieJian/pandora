      subroutine PVOIGT
     $(V,A,VOIGT)
C     Rudolf Loeser, 1978 Dec 14
C---- Drives PEYTRE.
C     !DASH
      save
C     !DASH
      real*8 A, V, VOIGT
      real*4 AA, P, VV
C     !DASH
      external  PEYTRE
      intrinsic abs
C
C     !BEG
      VV = abs(V)
      AA = A
      call PEYTRE (VV,AA,P)
      VOIGT = P
C     !END
C
      return
      end
