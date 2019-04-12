      subroutine SIGTUNA
     $(N,TAU,B,S,P,NP,TCRIT)
C
C     Rudolf Loeser, 1996 Feb 28
C---- Adds another S/B set to the sets in P.
C     NP is the count of sets in P.
C     (It is not necessary to check for storage overflow.)
C     !DASH
      save
C     !DASH
      real*8 B, P, S, TAU, TCRIT, ZERO
      integer I, J, N, NP
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
C               TAU(N), S(N), B(N), P(N,5)
      dimension TAU(*), S(*), B(*), P(N,*)
C
      call HI ('SIGTUNA')
C     !BEG
      J = NP+1
      do 100 I = 1,N
        if(TAU(I).lt.TCRIT) then
          P(I,J) = ZERO
        else
          call DIVIDE (S(I),B(I),P(I,J))
        end if
  100 continue
      NP = J
C     !END
      call BYE ('SIGTUNA')
C
      return
      end
