      subroutine CUANGO
     $(XLM,WLIN,FACT)
C
C     Rudolf Loeser, 2002 Sep 18
C---- Computes wavelength term for Hydrogen Lyman N/1 line wing opacity.
C     (This is version 2 of CUANGO.)
C     !DASH
      save
C     !DASH
      real*8 FACT, TWO, WLIN, XLM
C     !COM
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
      call HI ('CUANGO')
C     !BEG
      FACT = ((TWO*WLIN)/(XLM+WLIN))**2
C     !END
      call BYE ('CUANGO')
C
      return
      end
