      subroutine PXPNT1
     $(NM1,X,EI,EX,ERROR)
C     Rudolf Loeser, 1988 Nov 21
C---- Computes Lentz's algorithm, for PEXPINT.
C     !DASH
      save
C     !DASH
      real*8 AN, C, D, DEL, EI, EPS, EX, F, FI, ONE, TINY, X
      integer I, ITMAX, NM1
      logical ERROR
C     !DASH
      intrinsic abs
C
      data ITMAX /100/
      data EPS /1.D-14/
C     EPS is the desired accuracy, but should not be smaller then
C     the machine precision.
      data TINY /1.D-30/
      data ONE /1.D0/
C
C     !BEG
      F = ONE/X
      D = F
      C = ONE/TINY
      do 100 I = 1,ITMAX
        FI  = I
        AN  = I+NM1
        D   = ONE/(ONE+AN*D)
        C   = ONE+AN/C
        DEL = D*C
        F   = F*DEL
        D   = ONE/(X+FI*D)
        C   = X+FI/C
        DEL = D*C
        F   = F*DEL
        if(abs(DEL-ONE).lt.EPS) goto 101
  100 continue
      ERROR = .true.
  101 continue
      EI = F*EX
C     !END
C
      return
      end
