      subroutine FIVEDAG
     $(A,B,C,D,E,R,N,  U,  X,XL,INDX)
C     Rudolf Loeser, 1998 Jan 29
C---- Solves for a vector U of length N the five-diagonal system
C     represented by the vectors E, D, C, B, A, and R.
C     A, B, C, D, E, and R are input vectors, and are not modified.
C     (A(N), A(N-1), B(N), D(1), E(1), and E(2) are not used -
C     set them to 0).
C     X, XL, and INDX are scratch arrays.
C
C     Uses  b a n d e c   and   b a n b k s  from "Numerical Recipes".
C     !DASH
      save
C     !DASH
      real*8 A, B, C, D, E, R, SIG, U, X, XL
      integer I, INDX, N
C     !DASH
      external  BANDEC, BANBKS
C
      dimension A(N), B(N), C(N), D(N), E(N), R(N), U(N), X(N,5),
     $          XL(N,2), INDX(N)
C
C     !BEG
      do 100 I = 1,N
        X(I,1) = E(I)
        X(I,2) = D(I)
        X(I,3) = C(I)
        X(I,4) = B(I)
        X(I,5) = A(I)
        U(I)   = R(I)
  100 continue
C
      call BANDEC (X,N,2,2,N,5,XL,2,INDX,SIG)
      call BANBKS (X,N,2,2,N,5,XL,2,INDX,U)
C     !END
C
      return
      end
