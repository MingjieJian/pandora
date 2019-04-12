      subroutine MNMXD
     $(A,INC,N,XCL,IMIN,IMAX)
C     Rudolf Loeser, 1979 Apr 20
C---- See remarks in "SCANNER".
C     !DASH
      save
C     !DASH
      real*8 A, DELTA, DIV, XCL
      integer IMAX, IMIN, INC, K, N
C     !DASH
      external SCAND
C
      dimension A(*)
C
      data DELTA,DIV /0.D0, 0.D0/
C
C     !BEG
      call SCAND (A,INC,N,DELTA,DIV,XCL,IMIN,IMAX,K,K,K,K)
C     !END
C
      return
      end
