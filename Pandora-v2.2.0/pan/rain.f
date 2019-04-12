      subroutine RAIN
     $(N,NL,XNKS,XNKUW,RK,XNDS,XNDUW,RN)
C
C     Rudolf Loeser, 1975 Jul 30
C---- Computes ratios, for GORSE.
C     !DASH
      save
C     !DASH
      real*8 RK, RN, XNDS, XNDUW, XNKS, XNKUW, ZERO
      integer I, J, N, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external DIVIDE, HI, BYE
C
C               XNKS(N), XNKUW(N), XNDUW(N,NL), RN(N,NL), XNDS(N,NL),
      dimension XNKS(*), XNKUW(*), XNDUW(N,*),  RN(N,*),  XNDS(N,*),
C
C               RK(N)
     $          RK(*)
C
      call HI ('RAIN')
C     !BEG
      do 100 I = 1,N
        if(XNKUW(I).ne.ZERO) then
          call DIVIDE   (XNKUW(I),XNKS(I),RK(I))
        else
          RK(I) = ZERO
        end if
  100 continue
C
      do 102 J = 1,NL
        do 101 I = 1,N
          if(XNDUW(I,J).ne.ZERO) then
            call DIVIDE (XNDUW(I,J),XNDS(I,J),RN(I,J))
          else
            RN(I,J) = ZERO
          end if
  101   continue
  102 continue
C     !END
      call BYE ('RAIN')
C
      return
      end
