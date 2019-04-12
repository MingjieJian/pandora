      subroutine NUKIK
     $(IU,IL,CQL,FLU)
C
C     Rudolf Loeser, 1990 Oct 09
C---- Computes f(l,u) for Hydrogen.
C
C     Johnson, L.C.  1972, ApJ, 174, 227-236.
C
C     !DASH
      save
C     !DASH
      real*8 CON56, CQL, ELL, ELL2, FAC, FLU, G0, G1, G2, ONE, SUM, TUL,
     $       TUL2, TUL3, XUL, YOO2, YOO3
      integer IL, IU
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external KOLLA, RIGEL, HI, BYE
C
      call HI ('NUKIK')
C     !BEG
      call RIGEL (56,CON56)
      call KOLLA (IL,G0,G1,G2)
      ELL  = IL
      ELL2 = IL**2
      YOO2 = IU**2
      YOO3 = IU**3
      XUL  = ONE-ELL2/YOO2
      TUL  = XUL*CQL
      TUL2 = TUL**2
      TUL3 = TUL*TUL2
      FAC  = ELL/(YOO3*TUL3)
      SUM  = G0+G1/TUL+G2/TUL2
C
      FLU = CON56*FAC*SUM
C     !END
      call BYE ('NUKIK')
C
      return
      end
