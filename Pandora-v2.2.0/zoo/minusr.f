      subroutine MINUSR
     $(A,INC,N,LLT)
C     Rudolf Loeser, 1979 Dec 02
C---- See remarks in "SCANNER".
C     !DASH
      save
C     !DASH
      real*4 A, ZERO
      integer IMAX, IMIN, INC, K, LLT, N
C     !DASH
      external SCANR
C
      dimension A(*)
C
      data ZERO /0./
C
C     !BEG
      call SCANR (A,INC,N,ZERO,ZERO,ZERO,IMIN,IMAX,LLT,K,K,K)
C     !END
C
      return
      end
