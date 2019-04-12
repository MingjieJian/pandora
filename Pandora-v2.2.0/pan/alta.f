      subroutine ALTA
     $(G,THETA,CHI,H,G0,SIGMA)
C
C     Rudolf Loeser, 1982 Jun 09
C---- An alternative procedure for computing G0 and SIGMA,
C     for H I and HE II,
C     such that the Partition Function Q = G0 + SIGMA.
C
C     Copied from subroutine "PARTFN",
C     by Mats Carlsson, Oslo University,
C     Institute of Theoretical Astrophysics, May 1982.
C     !DASH
      save
C     !DASH
      real*8 ARG, CHI, FI2, G, G0, H, ONE, SIGMA, SUM, TEN, THETA, TRM,
     $       TWO, ZERO
      integer I, LIM
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT(11),TEN   )
C     !DASH
      external HI, BYE
C
      call HI ('ALTA')
C     !BEG
      SUM = ZERO
      LIM = H
      if(LIM.ge.2) then
        do 100 I = 2,LIM
          FI2 = I**2
          ARG = THETA*CHI*(ONE-ONE/FI2)
          TRM = TEN**(-ARG)
          SUM = SUM+FI2*TRM
  100   continue
      end if
C
      G0 = G
      SIGMA = TWO*SUM
C     !END
      call BYE ('ALTA')
C
      return
      end
