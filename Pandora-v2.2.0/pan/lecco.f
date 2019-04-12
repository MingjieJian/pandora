      subroutine LECCO
     $(XNE,RNH,PNH,CLM,XLM,CQV,ALB)
C
C     Rudolf Loeser, 1988 Dec 06
C---- Computes scattering albedo from Anderson's formula:
C     Anderson, L.S. (1989), Astrophys.J., eqn. 36;
C
C     with additional wavelength term, and
C     an ad-hoc depth-dependency based on NH.
C     !DASH
      save
C     !DASH
      real*8 ALB, CLM, CQV, FACW, ONE, PNH, REFLM, REFNE, REFWV, RNH,
     $       TRM, TRMD, XI, XLM, XNE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, HI, BYE
C
      data REFNE /1.07D14/
      data REFLM /5.D3/
      data REFWV /1.D4/
C
      call HI ('LECCO')
C     !BEG
C---- Anderson's Xi
      TRM = (XNE/REFNE)*((XLM/REFLM)**3)
      call DIVIDE (TRM, CQV, XI)
C---- Additional function of depth and wavelength
      FACW = (ONE+CLM*(REFWV/XLM))
      TRMD = PNH*RNH
C
      ALB = ONE/(ONE+(XI*FACW+TRMD))
C     !END
      call BYE ('LECCO')
C
      return
      end
