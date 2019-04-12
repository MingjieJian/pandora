      subroutine ANKE
     $(Z,XU,XL,VU,VL,GM,IU,IL,NL,PE,FE)
C
C     Rudolf Loeser, 1974 Mar 21
C---- Computes PE and FE for HAMMER.
C     !DASH
      save
C     !DASH
      real*8 FE, GM, GRAT, PE, RAT, VL, VU, VUVL, XL, XLVU, XU, XUVL, Z,
     $       ZERO
      integer IL, IU, NL
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
C               Z(NL,NL), GM(NL)
      dimension Z(NL,*),  GM(*)
C
      call HI ('ANKE')
C     !BEG
      VUVL = VU+VL
C
      XUVL = XU*VL
      if(XUVL.eq.ZERO) then
        RAT = ZERO
      else
        call DIVIDE (XUVL, VUVL, RAT)
      end if
C
      PE = Z(IU,IL)+RAT
C
      XLVU = XL*VU
      if(XLVU.eq.ZERO) then
        RAT = ZERO
      else
        call DIVIDE (XLVU, VUVL, RAT)
      end if
C
      call DIVIDE   (GM(IL), GM(IU), GRAT)
C
      FE = GRAT*(Z(IL,IU)+RAT)
C     !END
      call BYE ('ANKE')
C
      return
      end
