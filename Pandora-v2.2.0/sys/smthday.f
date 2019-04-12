      subroutine SMTHDAY
     $(SMITH, YEAR,OMNTH,DAY)
C     (Rudolf Loeser), 1998 Jun 10
C---- Computes Smithsonian Day (aka Modified Julian Day, MJD).
C
C
C---- Adapted from: subroutine SMTHDA, originally developed by
C
C     ROBERT E. BRIGGS
C
C     SMITHSONIAN ASTROPHYSICAL OBSERVATORY
C     (for the Satellite Tracking Program of the 1960's).
C
C     !DASH
      save
C     !DASH
      real*8 A, C, C1, C2, C3, C4, C5, C6, D, DAY, DZ, E, OMNTH, ONE,
     $       SMITH, X, Y, YEAR, ZERO
      integer J, M
C     !DASH
      dimension DZ(10)
C
      data      DZ /3.34D2, 3.04D2, 2.73D2, 2.43D2, 2.12D2, 1.81D2,
     $              1.51D2, 1.2D2, 9.D1, 5.9D1/
C
      data      ZERO,ONE /0.D0, 1.D0/
      data      C1,C2,C3,C4 /8.D-1, 1.9D3, 3.6525D2, 9.D-1/
      data      C5,C6 /3.1D1, 1.5018D4/
C
C     !BEG
      Y = YEAR
      M = OMNTH+C1
      D = DAY
      A = Y-C2
      IF(A)2,1,1
    1 Y = A
    2 X = C3*Y+C4
      J = X
      A = J
      IF(M-2)3,5,6
    3 E = ZERO
      GO TO 4
    5 E = C5
      GO TO 4
    6 IF(X-A-C1)7,7,8
    7 C = ZERO
      GO TO 9
    8 C = ONE
    9 J = 13-M
      E = DZ(J)+C
    4 SMITH = C6+A+D+E
C     !END
C
      return
      end
