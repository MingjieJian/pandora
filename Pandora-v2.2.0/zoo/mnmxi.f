      subroutine MNMXI
     $(A,INC,N,XCL,IMIN,IMAX)
C     Rudolf Loeser, 1979 Apr 20
C---- See remarks in "SCANNER".
C     !DASH
      save
C     !DASH
      integer A, DELTA, DIV, IMAX, IMIN, INC, K, N, XCL
C     !DASH
      external SCANI
C
      dimension A(*)
C
      data DELTA,DIV /0, 0/
C
C     !BEG
      call SCANI (A,INC,N,DELTA,DIV,XCL,IMIN,IMAX,K,K,K,K)
C     !END
C
      return
      end
