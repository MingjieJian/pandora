      subroutine PLUSD
     $(A,INC,N,LGT)
C     Rudolf Loeser, 1979 Dec 02
C---- See remarks in "SCANNER".
C     !DASH
      save
C     !DASH
      real*8 A, ZERO
      integer IMAX, IMIN, INC, K, LGT, N
C     !DASH
      external SCAND
C
      dimension A(*)
C
      data ZERO /0.D0/
C
C     !BEG
      call SCAND (A,INC,N,ZERO,ZERO,ZERO,IMIN,IMAX,K,K,LGT,K)
C     !END
C
      return
      end
