      subroutine KONSTR
     $(A,INC,N,XCL,FLAG)
C     Rudolf Loeser, 1979 Apr 20
C---- See remarks in "SCANNER".
C     !DASH
      save
C     !DASH
      real*4 A, DELTA, DIV, XCL
      integer IMAX, IMIN, INC, K, KXCL, N
      logical FLAG
C     !DASH
      external SCANR
C
      dimension A(*)
C
      data DELTA,DIV /0., 0./
C
C     !BEG
      call SCANR (A,INC,N,DELTA,DIV,XCL,IMIN,IMAX,K,K,K,KXCL)
      FLAG = N.eq.KXCL
C     !END
C
      return
      end
