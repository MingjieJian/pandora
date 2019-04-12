      subroutine MILDRED
     $(H1,XLM,OPAC)
C
C     Rudolf Loeser, 1988 Oct 27
C---- Computes Rayleigh scattering opacity.
C     (This is version 3 of MILDRED.)
C     !DASH
      save
C     !DASH
      real*8 FAC, H1, OPAC, R, RSL, RSS, RYDBRG, XLM, XLML, XLMS, ZERO
      integer jummy
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- APULIA      as of 1994 Nov 02
      real*8      RAYSLM
      common      /APULIA/ RAYSLM
C     Wavelength crossover for Rayleigh scattering computations.
C     .
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 4),RYDBRG)
C     !DASH
      external LININT, HI, BYE
C
      dimension XLMS(12), RSS(12), XLML(17), RSL(17)
C
      data XLMS /
     $ 0.00D0, 2.28D2, 2.53D2, 2.85D2, 3.25D2, 3.80D2, 4.56D2,
     $ 5.06D2, 5.70D2, 6.51D2, 7.60D2, 0.00D0/
      data RSS /
     $ 5.D-1, 6.D-1, 6.2D-1, 6.5D-1, 6.8D-1, 7.2D-1, 7.9D-1,
     $ 8.3D-1, 9.D-1, 9.8D-1, 1.11D0, 1.33D0/
      data XLML /
     $ 0.000D0, 1.520D3, 1.628D3, 1.753D3, 1.900D3, 2.072D3, 2.280D3,
     $ 2.539D3, 2.850D3, 3.257D3, 3.800D3, 4.560D3, 5.700D3, 7.600D3,
     $ 9.120D3, 1.140D4, 2.280D4/
      data RSL /
     $ 9.36D-25, 4.5D-25, 2.41D-25, 1.37D-25, 8.02D-26, 4.76D-26,
     $ 2.8D-26, 1.63D-26, 9.22D-27, 4.97D-27, 2.44D-27, 1.14D-27,
     $ 4.46D-28, 1.36D-28, 6.48D-29, 2.65D-29, 1.61D-30/
C
      data FAC /1.D-24/
C     !EJECT
C
      call HI ('MILDRED')
C     !BEG
C---- Set up uninitialized wavelength values
      if(XLMS(12).eq.ZERO) then
        XLMS(12) = RYDBRG
      end if
      if(XLML( 1).eq.ZERO) then
        XLML( 1) = RAYSLM
      end if
C
      if(XLM.lt.ZERO) then
C----   Bad wavelength value
        OPAC = ZERO
C
      else if(XLM.le.XLMS(12)) then
C----   Short-wavelengths table
        call LININT (XLMS,1,RSS,1,12, XLM,R, 1,1,jummy)
        OPAC = R*H1*FAC
C
      else if(XLM.lt.XLML(1)) then
C----   Region covered in H Lyman alpha background opacity
        OPAC = ZERO
C
      else if(XLM.le.XLML(17)) then
C----   Long-wavelengths table
        call LININT (XLML,1,RSL,1,17, XLM,R, 2,1,jummy)
        OPAC = R*H1
C
      else
C----   No data at long wavelengths
        OPAC = RSL(17)*H1
      end if
C     !END
      call BYE ('MILDRED')
C
      return
      end
