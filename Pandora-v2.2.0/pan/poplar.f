      subroutine POPLAR
     $(N,M,RR,NN,V,WN)
C
C     Rudolf Loeser, 1968 Jun 04 (revised 2000 Jan 27)
C---- Special matrix multiplication routine.
C     It computes V*R=WN, where V is of order N*M and R is of order
C     M*N. However, R has only two non-zero elements in each row, and
C     is represented by the two vectors RR and NN, such that NN(I) is
C     the index of the first non-zero element in row I, whose value
C     is RR(I). The second non-zero element occurs at NN(I)+1
C     (NN(I) .lt. N) and its value is 1-RR(I). (If NN(I)=N, there
C     is only one non-zero element, RR(I).)
C     !DASH
      save
C     !DASH
      real*8 ONE, R, RR, V, WN
      integer I, J, JR, M, N, NN
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ZERO1, HI, BYE
C
C               RR(M), NN(M), V(N,M), WN(N,N)
      dimension RR(*), NN(*), V(N,*), WN(N,*)
C     !EJECT
C
      call HI ('POPLAR')
C     !BEG
      call ZERO1 (WN,(N**2))
C
      do 100 JR = 1,M
        J = NN(JR)
        R = RR(JR)
        do 100 I = 1,N
          WN(I,J) = WN(I,J) + V(I,JR)*R
          if(J.lt.N) then
            WN(I,J+1) = WN(I,J+1) + V(I,JR)*(ONE-R)
          end if
  100   continue
  101 continue
C     !END
      call BYE ('POPLAR')
C
      return
      end
