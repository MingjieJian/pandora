      subroutine NUNNI
     $(YOO,ELL,TE,H)
C
C     Rudolf Loeser, 1990 Dec 07
C---- Computes H, for the Vriens & Smeets model of the Hydrogen atom.
C     !DASH
      save
C     !DASH
      real*8 BRK, C1, C2, C3, C4, C5, CON58, DEN, EL3, ELEVEN, ELL, H,
     $       HALF, ONE, PU, RUML, SIX, TE, TEE, THREE, TL, UML, YOO
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 4),THREE )
      equivalence (DLIT( 7),SIX   )
C     !DASH
      external RIGEL, HI, BYE
C
      data C1,C2,C3,C4,C5 /1.6D0, 3.D-1, 1.5D0, 8.D-1, 6.D-1/
      data ELEVEN /1.1D1/
C
      call HI ('NUNNI')
C     !BEG
      call RIGEL  (58,CON58)
C
      PU   = YOO**C3
      UML  = YOO-ELL
      RUML = sqrt(UML)
      DEN  = SIX+C1*YOO*UML+C2/(UML**2)+((C4*PU)/RUML)*(UML-C5)
C
      TEE = TE/CON58
      EL3 = ELL**3
      TL  = log(ONE+EL3*TEE)
      BRK = THREE+ELEVEN*((UML/ELL)**2)
C
      H = ((TL/TEE)*BRK)/DEN
C     !END
      call BYE ('NUNNI')
C
      return
      end
