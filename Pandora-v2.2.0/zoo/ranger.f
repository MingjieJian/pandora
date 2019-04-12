      subroutine RANGER
     $(A,INC,N,DELTA,DIV,KEQ)
C     Rudolf Loeser, 1979 Dec 02
C---- See remarks in "SCANNER".
C     !DASH
      save
C     !DASH
      real*4 A, DELTA, DIV
      integer IMAX, IMIN, INC, K, KEQ, N
C     !DASH
      external SCANR
C
      dimension A(*)
C
C     !BEG
      call SCANR (A,INC,N,DELTA,DIV,DIV,IMIN,IMAX,K,KEQ,K,KEQ)
C     !END
C
      return
      end
