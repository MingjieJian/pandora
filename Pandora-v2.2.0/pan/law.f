      subroutine LAW
     $(M,DEL,F,G,R,A,B,C,D)
C
C     Rudolf Loeser, 1998 May 20
C---- Computes "method-2" A, B, C, D, for 4-diagonal solution.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, D, DEL, F, FT, G, HALF, R, ZERO
      integer I, J, K, L, M
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
C     !DASH
      external HI, BYE
C
C               DEL(N), F(N), G(N), R(N), A(N), B(N), C(N), D(N)
      dimension DEL(*), F(*), G(*), R(*), A(*), B(*), C(*), D(*)
C     !EJECT
C
      call HI ('LAW')
C     !BEG
      A(1) = ZERO
      D(1) = ZERO
C
      B(1) = R(1)
      C(1) = ZERO
C----
      A(2) = -F(2)/(DEL(2)*(DEL(2)+DEL(3)))
      D(2) =  ZERO
C
      FT   =  F(1)/(DEL(2)**2)
      B(2) =  HALF*R(2)+G(2)/DEL(2)+FT
      C(2) =  HALF*R(1)-G(1)/DEL(2)-FT-A(2)
C----
      do 100 J = 3,(M-1)
        I = J-1
        K = J+1
C
        A(J) = -F(J)/(DEL(J)*(DEL(J)+DEL(K)))
        D(J) = -F(I)/(DEL(J)*(DEL(I)+DEL(J)))
C
        B(J) =  HALF*R(J)+G(J)/DEL(J)-D(J)
        C(J) =  HALF*R(I)-G(I)/DEL(J)-A(J)
  100 continue
C----
      L = M-1
C
      A(M) =  ZERO
      D(M) = -F(L)/(DEL(M)*(DEL(L)+DEL(M)))
C
      FT   =  F(M)/(DEL(M)**2)
      B(M) =  HALF*R(M)+G(M)/DEL(M)-FT-D(M)
      C(M) =  HALF*R(L)-G(L)/DEL(M)+FT
C     !END
      call BYE ('LAW')
C
      return
      end
