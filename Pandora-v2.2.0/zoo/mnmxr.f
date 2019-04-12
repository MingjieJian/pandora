      subroutine MNMXR
     $(A,INC,N,XCL,IMIN,IMAX)
C     Rudolf Loeser, 1979 Apr 20
C---- See remarks in "SCANNER".
C     !DASH
      save
C     !DASH
      real*4 A, DELTA, DIV, XCL
      integer IMAX, IMIN, INC, K, N
C     !DASH
      external SCANR
C
      dimension A(*)
C
      data DELTA,DIV /0., 0./
C
C     !BEG
      call SCANR (A,INC,N,DELTA,DIV,XCL,IMIN,IMAX,K,K,K,K)
C     !END
C
      return
      end
