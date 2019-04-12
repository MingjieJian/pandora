      subroutine RANGEI
     $(A,INC,N,DELTA,DIV,KEQ)
C     Rudolf Loeser, 1979 Dec 02
C---- See remarks in "SCANNER".
C     !DASH
      save
C     !DASH
      integer A, DELTA, DIV, IMAX, IMIN, INC, K, KEQ, N
C     !DASH
      external SCANI
C
      dimension A(*)
C
C     !BEG
      call SCANI (A,INC,N,DELTA,DIV,DIV,IMIN,IMAX,K,KEQ,K,KEQ)
C     !END
C
      return
      end
