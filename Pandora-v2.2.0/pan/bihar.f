      subroutine BIHAR
     $(N,NP,EM,C)
C
C     Rudolf Loeser, 1981 Nov 02
C---- Computes weights for integrations over Shell Rays.
C     !DASH
      save
C     !DASH
      real*8 C, EM, HALF
      integer I, J, LIM, N, NP, NPP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
C     !DASH
      external MUSHAI, HI,BYE
C
C               EM(N,NP), C(N,NP)
      dimension EM(N,*),  C(N,*)
C
      call HI ('BIHAR')
C     !BEG
      do 101 I = 1,N
C
        call MUSHAI (I, NPP)
        if(NPP.eq.1) then
          C(I,1) = EM(I,1)
C
        else if(NPP.gt.1) then
          J = 1
          C(I,J) = HALF*(EM(I,J)-EM(I,J+1))
C
          LIM = NPP-1
          if(LIM.ge.2) then
            do 100 J = 2,LIM
              C(I,J) = HALF*(EM(I,J-1)-EM(I,J+1))
  100       continue
          end if
C
          J = NPP
          C(I,J) = HALF*(EM(I,J-1)+EM(I,J))
        end if
C
  101 continue
C     !END
      call BYE ('BIHAR')
C
      return
      end
