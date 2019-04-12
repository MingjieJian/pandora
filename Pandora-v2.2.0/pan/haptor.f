      subroutine HAPTOR
     $(Y,H)
C
C     Rudolf Loeser, 1991 Jan 03
C---- Computes H(y), for ERIKA.
C     (This is version 2 of HAPTOR.)
C     !DASH
      save
C     !DASH
      real*8 FAC, H, TWO, Y
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
      external HI, BYE
C
      data FAC /2.8215D7/
C
      call HI ('HAPTOR')
C     !BEG
      H = (FAC/R)*((TWO*Y-DNU)**2)
C     !END
      call BYE ('HAPTOR')
C
      return
      end
