      subroutine KONSTC
     $(A,INC,N,XCL,FLAG)
C     Rudolf Loeser, 1979 Apr 20
C---- See remarks in "SCANNER".
C     !DASH
      save
C     !DASH
      integer IMAX, IMIN, INC, K, KXCL, N
      logical FLAG
      character A*(*), DIV*1, XCL*(*)
C     !DASH
      external SCANC
C
      dimension A(*)
C
      data DIV /'L'/
C
C     !BEG
      call SCANC (A,INC,N,DIV,XCL,IMIN,IMAX,K,K,K,KXCL)
      FLAG = N.eq.KXCL
C     !END
C
      return
      end
