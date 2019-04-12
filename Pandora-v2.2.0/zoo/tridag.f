      subroutine TRIDAG
     $(A,B,C,R,U,N,TMP)
C
C     Rudolf Loeser, 1988 Jun 22
C
C---- From  "Numerical Recipes"
C     by Press, Flannery, Teukolsky & Vetterling (1986)
C     Cambridge University Press
C
C---- Solves for a vector U of length N the tridiagonal system
C     represented by the vectors A, B, C and R.
C     A, B, C and R are input vectors, and are not modified.
C     (A(1) and C(N) are not used.)
C     !DASH
      save
C     !DASH
      real*8 A, B, BET, C, R, TMP, U, ZERO
      integer J, N
C     !DASH
      external  ABORT
C
C               A(N), B(N), C(N), R(N), U(N), TMP(N)
      dimension A(*), B(*), C(*), R(*), U(*), TMP(*)
C
      data  ZERO /0.D0/
C
C     !BEG
      BET = B(1)
      if(BET.eq.ZERO) then
        write (*,100) 1
  100   format(' ','Error in TRIDAG: divisor = 0 for J =',I6)
        call ABORT
      end if
C
      U(1) = R(1)/BET
C
      do 101 J = 2,N
        TMP(J) = C(J-1)/BET
        BET    = B(J)-A(J)*TMP(J)
        if(BET.eq.ZERO) then
          write (*,100) J
          call ABORT
        end if
C
        U(J) = (R(J)-A(J)*U(J-1))/BET
  101 continue
C
      do 102 J = (N-1),1,-1
        U(J) = U(J)-TMP(J+1)*U(J+1)
  102 continue
C     !END
C
      return
      end
