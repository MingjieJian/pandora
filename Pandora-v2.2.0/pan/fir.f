      subroutine FIR
     $(W,N,I,DT,SE2,SE3)
C
C     Rudolf Loeser, 1971 Jul 09 (revised 2000 Jan 25)
C---- Modifies the I-th row of a finite matrix, to make it apply to
C     the semi-infinite case, for the RT weight matrix calculation.
C     !DASH
      save
C     !DASH
      real*8 DT, E2, E3, HALF, SE2, SE3, TERM, TM, TWO, W
      integer I, J, JM, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external  WALNUT, DIVIDE, HI, BYE
      intrinsic abs
C
C               W(N,N), DT(2*N), SE2(2*N), SE3(2*N)
      dimension W(N,*), DT(*),   SE2(*),   SE3(*)
C
      call HI ('FIR')
C     !BEG
      J  = N
      JM = J-1
C
      call WALNUT (2,J,DT,SE2,E2)
      call WALNUT (3,J,DT,SE3,E3)
C
      TM = abs(DT(J)-DT(JM))
      call DIVIDE (E3,(TWO*TM),TERM)
C
      W(I,J ) = W(I,J )+TERM+HALF*E2
      W(I,JM) = W(I,JM)-TERM
C     !END
      call BYE ('FIR')
C
      return
      end
