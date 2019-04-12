      subroutine ASSAM
     $(N,NP,EM,C)
C
C     Rudolf Loeser, 1982 Apr 22
C---- Computes weights for integrations over Shell Rays.
C     Special case - tangent ray is included, and no rays are skipped.
C     !DASH
      save
C     !DASH
      real*8 C, EM, HALF
      integer I, J, LIM, N, NP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
C     !DASH
      external HI, BYE
C
C               EM(N,NP), C(N,NP)
      dimension EM(N,*),  C(N,*)
C
      call HI ('ASSAM')
C     !BEG
      do 101 I = 1,NP
C
        if(I.eq.1) then
          LIM = N-2
        else
          LIM = N-I
        end if
C
        J = 1
        C(I,J) = HALF*(EM(I,J)-EM(I,J+1))
C
        if(LIM.ge.2) then
          do 100 J = 2,LIM
            C(I,J) = HALF*(EM(I,J-1)-EM(I,J+1))
  100     continue
        end if
C
        J = LIM+1
        C(I,J) = HALF*EM(I,J-1)
        if(I.eq.1) then
          C(I,J) = C(I,J)+HALF*EM(I,J)
        end if
C
  101 continue
C     !END
      call BYE ('ASSAM')
C
      return
      end
