      subroutine RAPAR
     $(IL,IG,A,TAU)
C
C     Rudolf Loeser, 1979 Dec 02
C---- Repairs a bad interval in A, for SENTA.
C     !DASH
      save
C     !DASH
      real*8 A, DTAU, OD, ONE, R, SIGL, SILL, TAU, TEN, W
      integer IG, IL, J
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT(11),TEN   )
C     !DASH
      external DIVIDE, HI, BYE
C
      dimension A(*), TAU(*)
C
      call HI ('RAPAR')
C     !BEG
      DTAU = TAU(IG)-TAU(IL)
      call DIVIDE (ONE,DTAU,OD)
C
      SILL = log10(A(IL))
      SIGL = log10(A(IG))
C
      do 100 J = (IL+1),(IG-1)
        W    = (TAU(IG)-TAU(J ))*OD
        R    = (TAU(J )-TAU(IL))*OD
        A(J) = TEN**(W*SILL+R*SIGL)
  100 continue
C     !END
      call BYE ('RAPAR')
C
      return
      end
