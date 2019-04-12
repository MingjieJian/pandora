      subroutine MINUSI
     $(A,INC,N,LLT)
C     Rudolf Loeser, 1979 Dec 02
C---- See remarks in "SCANNER".
C     !DASH
      save
C     !DASH
      integer A, IMAX, IMIN, INC, K, LLT, N, ZERO
C     !DASH
      external SCANI
C
      dimension A(*)
C
      data ZERO /0/
C
C     !BEG
      call SCANI (A,INC,N,ZERO,ZERO,ZERO,IMIN,IMAX,LLT,K,K,K)
C     !END
C
      return
      end
