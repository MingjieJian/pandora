      subroutine ERIKA
     $(VEC,Y,F)
C
C     Rudolf Loeser, 1992 Jan 03
C---- Computes values of the integrand for DOMINO via ADAIR.
C     (This is version 2 of ERIKA.)
C     !DASH
      save
C     !DASH
      real*8 ARG, BETA0, BETA1, BK0, BK1, EX, F, G, G0, G1, H, RT, TWO,
     $       VEC, Y
C     !COM
C---- OLIVIA      as of 2006 Mar 08
      real*8      XNUKH,RBAR,DNU,PT,R,TERJ
      integer     IUCE,ILCE
      common      /OLIVIA/ XNUKH,RBAR,DNU,PT,R
      common      /OLIVIB/ TERJ
      common      /OLIVIC/ IUCE,ILCE
C     Parameters for ERIKA: calculation of Collisional Ionization
C     Integral, for the impact-parameter method.
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external  HAPTOR, FROOT, BESSEL, HI, BYE
      intrinsic min
C
      call HI ('ERIKA')
C     !BEG
C---- Compute H(y)
      call HAPTOR (Y, H)
C---- Compute beta0
      RT    = sqrt(Y/XNUKH)
      BETA0 = (RT*DNU*RBAR)/(TWO*Y-DNU)
C---- Compute beta1
      call FROOT  (Y, H, BETA1)
C---- Compute G
      call BESSEL (BETA0, BK0, BK1)
      G0 = BETA0*BK0*BK1
      call BESSEL (BETA1, BK0, BK1)
      G1 = BETA1*(BK0*BK1+BETA1*(H/TWO))
C
      G = min(G0,G1)
C---- Compute F
      ARG = PT*(Y-DNU)
      EX  = exp(-ARG)
C
      F = G*EX
C     !END
      call BYE ('ERIKA')
C
      return
      end
