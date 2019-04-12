      subroutine MNMXC
     $(A,INC,N,XCL,IMIN,IMAX)
C     Rudolf Loeser, 1979 Apr 20
C---- See remarks in "SCANNER".
C     !DASH
      save
C     !DASH
      integer IMAX, IMIN, INC, K, N
      character A*(*), DIV*1, XCL*(*)
C     !DASH
      external SCANC
C
      dimension A(*)
C
      data DIV /'L'/
C
C     !BEG
      call SCANC (A,INC,N,DIV,XCL,IMIN,IMAX,K,K,K,K)
C     !END
C
      return
      end
