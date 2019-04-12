      subroutine FOURDAG
     $(A,B,C,D,R,N,  U,  X,XL,INDX)
C     Rudolf Loeser, 1997 Aug 27
C---- Solves for a vector U of length N the four-diagonal system
C     represented by the vectors D, C, B, A, and R.
C     A, B, C, D, and R are input vectors, and are not modified.
C     (A(N), C(1), D(1), and D(2) are not used - set them to 0).
C     X, XL, and INDX are scratch arrays.
C
C     Uses  b a n d e c   and   b a n b k s  from "Numerical Recipes".
C     !DASH
      save
C     !DASH
      real*8 A, B, C, D, R, SIG, U, X, XL
      integer I, INDX, N
C     !DASH
      external  BANDEC, BANBKS
C
      dimension A(N), B(N), C(N), D(N), R(N), U(N), X(N,4), XL(N,2),
     $          INDX(N)
C
C     !BEG
      do 100 I = 1,N
        X(I,1) = D(I)
        X(I,2) = C(I)
        X(I,3) = B(I)
        X(I,4) = A(I)
        U(I)   = R(I)
  100 continue
C
      call BANDEC (X,N,2,1,N,4,XL,2,INDX,SIG)
      call BANBKS (X,N,2,1,N,4,XL,2,INDX,U)
C     !END
C
      return
      end
