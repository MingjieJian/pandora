      subroutine NAUGHTI
     $(A,INC,N,FLAG)
C     Rudolf Loeser, 1979 Apr 20
C---- See remarks in "SCANNER".
C     !DASH
      save
C     !DASH
      integer A, DELTA, DIV, IMAX, IMIN, INC, K, KXCL, N, XCL
      logical FLAG
C     !DASH
      external SCANI
C
      dimension A(*)
C
      data DELTA,DIV,XCL /0, 0, 0/
C
C     !BEG
      call SCANI (A,INC,N,DELTA,DIV,XCL,IMIN,IMAX,K,K,K,KXCL)
      FLAG = N.eq.KXCL
C     !END
C
      return
      end
