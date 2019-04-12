      subroutine JAW
     $(M,DEL,F,G,R,A,B,C,D)
C
C     Rudolf Loeser, 1998 May 20
C---- Computes "method-1" A, B, C, D, for 4-diagonal solution.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, D, DEL, F, G, HALF, ONE, R, TWO, ZERO
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
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external HI, BYE
C
C               DEL(N), F(N), G(N), R(N), A(N), B(N), C(N), D(N)
      dimension DEL(*), F(*), G(*), R(*), A(*), B(*), C(*), D(*)
C     !EJECT
C
      call HI ('JAW')
C     !BEG
      A(1) = ZERO
      D(1) = ZERO
C
      B(1) = R(1)
      C(1) = ZERO
C----
      A(2) = -HALF*F(2)/(DEL(2)*DEL(3))
      D(2) =  ZERO
C
      B(2) =  HALF*R(2)+G(2)/DEL(2)
     $       +HALF*(F(2)/DEL(3)-F(2)/DEL(2))/DEL(2)
C
      C(2) =  HALF*R(1)-G(1)/DEL(2)
     $       +HALF*(F(2)-TWO*F(1))/(DEL(2)**2)
C----
      do 100 J = 3,(M-1)
        I = J-1
        K = J+1
C
        A(J) = -HALF*F(J)/(DEL(J)*DEL(K))
        D(J) = -HALF*F(I)/(DEL(J)*DEL(I))
C
        B(J) =  HALF*R(J)+G(J)/DEL(J)
     $         +HALF*(F(J)/DEL(K)-F(J)/DEL(J)+F(I)/DEL(J))/DEL(J)
C
        C(J) =  HALF*R(I)-G(I)/DEL(J)
     $         +HALF*(F(J)-F(I)*(ONE-DEL(J)/DEL(I)))/(DEL(J)**2)
  100 continue
C----
      L = M-1
C
      A(M) =  ZERO
      D(M) = -HALF*F(L)/(DEL(M)*DEL(L))
C
      B(M) =  HALF*R(M)+G(M)/DEL(M)
     $       -(F(M)-HALF*F(L))/(DEL(M)**2)
C
      C(M) =  HALF*R(L)-G(L)/DEL(M)
     $       +(F(M)-HALF*F(L)*(ONE-DEL(M)/DEL(L)))/(DEL(M)**2)
C     !END
      call BYE ('JAW')
C
      return
      end
