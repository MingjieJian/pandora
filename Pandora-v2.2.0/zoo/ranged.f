      subroutine RANGED
     $(A,INC,N,DELTA,DIV,KEQ)
C     Rudolf Loeser, 1979 Dec 02
C---- See remarks in "SCANNER".
C     !DASH
      save
C     !DASH
      real*8 A, DELTA, DIV
      integer IMAX, IMIN, INC, K, KEQ, N
C     !DASH
      external SCAND
C
      dimension A(*)
C
C     !BEG
      call SCAND (A,INC,N,DELTA,DIV,DIV,IMIN,IMAX,K,KEQ,K,KEQ)
C     !END
C
      return
      end
