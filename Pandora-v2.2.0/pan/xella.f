      subroutine XELLA
     $(TE,XFX)
C
C     Rudolf Loeser, 1995 Apr 27
C---- Computes a factor for CO collisional damping, for LARD.
C     (This is version 3 of XELLA.)
C     !DASH
      save
C     !DASH
      real*8 A, B, C, D, DNORM, E, F, RTX, TE, X, XFX
C     !DASH
      external HI, BYE
      data A, B, C, D, E /3.32D1, -1.25D1, 1.06D1, -8.78D0, 3.63D0/
      data DNORM /5.D3/
C
      call HI ('XELLA')
C     !BEG
      X = TE/DNORM
      F = A+X*(B+X*(C+X*(D+X*E)))
      RTX = sqrt(X)
      XFX = F*RTX
C     !END
      call BYE ('XELLA')
C
      return
      end
