      subroutine FARRAH
     $(N,K,FJN,XJNU,FAB,FJR)
C
C     Rudolf Loeser, 1981 Dec 18
C---- Computes FJR, a PRD term.
C     (This is version 2 of FARRAH.)
C     !DASH
      save
C     !DASH
      real*8 FAB, FJN, FJR, ONE, RAT, XJNU, ZERO
      integer I, J, K, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, HI, BYE
C
C               FJN(N,K), XJNU(N,K), FAB(N,K), FJR(N,K)
      dimension FJN(N,*), XJNU(N,*), FAB(N,*), FJR(N,*)
C
      call HI ('FARRAH')
C     !BEG
      do 101 J = 1,K
        do 100 I = 1,N
          if(FJN(I,J).ne.ZERO) then
            call DIVIDE (FJN(I,J),(XJNU(I,J)*FAB(I,J)),RAT)
            FJR(I,J) = ONE-RAT
          else
            FJR(I,J) = ZERO
          end if
  100   continue
  101 continue
C     !END
      call BYE ('FARRAH')
C
      return
      end
