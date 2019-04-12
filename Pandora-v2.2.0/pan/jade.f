      subroutine JADE
     $(N,X,B,S)
C
C     Rudolf Loeser, 1978 Apr 05
C---- Computes S directly, for WHALE.
C     !DASH
      save
C     !DASH
      real*8 B, S, X
      integer J, N
C     !DASH
      external ZERO1, ARRINC, HI, BYE
C
C               X(N,N), B(N), S(N)
      dimension X(N,*), B(*), S(*)
C
C
      call HI ('JADE')
C     !BEG
      call ZERO1    (S,N)
      do 100 J = 1,N
        call ARRINC (X(1,J),B(J),S,N)
  100 continue
C     !END
      call BYE ('JADE')
C
      return
      end
