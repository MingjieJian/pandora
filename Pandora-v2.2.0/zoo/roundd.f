      subroutine ROUNDD
     $(VALUE,ROUNDED)
C
C     Rudolf Loeser, 2003 Jul 24
C---- Computes ROUNDED, which is VALUE rounded to the nearest integer.
C     !DASH
      save
C     !DASH
      real*8 ONE, ROUNDED, VALUE, X, Y, Z
C     !DASH
      intrinsic abs, aint, sign
C
      data ONE /1.D0/
C
C     !BEG
      Y = abs(VALUE)
      X = aint(Y)
      Z = X+ONE
      if((Y-X).gt.(Z-Y)) then
        ROUNDED = sign(Z,VALUE)
      else
        ROUNDED = sign(X,VALUE)
      end if
C     !END
C
      return
      end
