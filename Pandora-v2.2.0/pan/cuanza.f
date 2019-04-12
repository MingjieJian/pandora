      subroutine CUANZA
     $(N,XLM,Z)
C
C     Rudolf Loeser, 2002 Sep 18
C---- Computes wavelength term for Hydrogen Lyman N/1 line wing opacity.
C     (This is version 2 of CUANZA.)
C     !DASH
      save
C     !DASH
      real*8 ONE, XLM, Z, ZERO
      integer N
C     !COM
C---- APULIA      as of 1994 Nov 02
      real*8      RAYSLM
      common      /APULIA/ RAYSLM
C     Wavelength crossover for Rayleigh scattering computations.
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
      call HI ('CUANZA')
C     !BEG
      if((XLM.ge.RAYSLM).and.(N.eq.2)) then
        Z = ZERO
      else
        Z = ONE
      end if
C     !END
      call BYE ('CUANZA')
C
      return
      end
