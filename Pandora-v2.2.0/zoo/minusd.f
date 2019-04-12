      subroutine MINUSD
     $(A,INC,N,LLT)
C     Rudolf Loeser, 1979 Dec 02
C---- See remarks in "SCANNER".
C     !DASH
      save
C     !DASH
      real*8 A, ZERO
      integer IMAX, IMIN, INC, K, LLT, N
C     !DASH
      external SCAND
C
      dimension A(*)
C
      data ZERO /0.D0/
C
C     !BEG
      call SCAND (A,INC,N,ZERO,ZERO,ZERO,IMIN,IMAX,LLT,K,K,K)
C     !END
C
      return
      end
