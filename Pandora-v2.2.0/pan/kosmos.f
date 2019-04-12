      subroutine KOSMOS
C
C     Rudolf Loeser, 1998 Jan 20
C---- Sets up constants in SHAMAN.
C     !DASH
      save
C     !DASH
      real*8 ANGPCM, AUNIT, BOHRAD, BOLZMN, CLIGHT, CMPKM, DEBYE,
     $       DGPRAD, EIGHT, ELCHRG, ELMASS, ERGPEV, EVPK, F15, FREQEV,
     $       FRQUNT, HEMASS, HYLYK, HYMASS, PI, PLANCK, ROOT2, ROOT3,
     $       ROOTPI, RYDBRG, SOLDIA, SOLRAD, SOLSGR, STFBLZ, THREE, TWO,
     $       XLOGE, XMBARN, dummy1, dummy2
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
C     .
      equivalence
     $(PCON( 1),PLANCK),(PCON( 2),BOLZMN),(PCON( 3),CLIGHT),
     $(PCON( 4),RYDBRG),(PCON( 5),HEMASS),(PCON( 6),ELMASS),
     $(PCON( 7),FREQEV),(PCON( 8),HYMASS),(PCON( 9),SOLSGR),
     $(PCON(10),SOLRAD),(PCON(11),AUNIT ),(PCON(12),ELCHRG),
     $(PCON(13),ERGPEV),(PCON(14),EVPK  ),(PCON(15),STFBLZ),
     $(PCON(16),BOHRAD),(PCON(17),HYLYK ),(PCON(18),SOLDIA)
      equivalence
     $(TUNI( 1),PI    ),(TUNI( 2),ROOTPI),(TUNI( 3),FRQUNT),
     $(TUNI( 4),XMBARN),(TUNI( 5),CMPKM ),(TUNI( 6),ANGPCM),
     $(TUNI( 7),ROOT2 ),(TUNI( 8),ROOT3 ),(TUNI( 9),XLOGE ),
     $(TUNI(10),DGPRAD),(TUNI(11),DEBYE )
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 4),THREE )
      equivalence (DLIT( 9),EIGHT )
C     !DASH
      external HYDATA, HI, BYE
C
      data F15 /1.5D1/
C     !EJECT
C---- Planck's constant
      data PLANCK                       /6.6260755D-27/
C---- Boltzmann's constant
      data BOLZMN                       /1.380658D-16/
C---- Velocity of light
      data CLIGHT                       /2.99792458D+10/
C---- Mass of Helium atom
      data HEMASS                       /4.0026D0/
C---- Mass of electron
      data ELMASS                       /9.1093897D-28/
C---- Nu-tilde (frequency of 1 eV)
      data FREQEV                       /2.417965D+14/
C---- Mass of Hydrogen atom
      data HYMASS                       /1.67333D-24/
C---- Surface gravity of Sun
      data SOLSGR                       /2.7398D+4/
C---- Solar radius (cm)
      data SOLRAD                       /6.9599D10/
C---- Astronomical unit (cm)
      data AUNIT                        /1.495979D13/
C---- Electron charge
      data ELCHRG                       /4.80320680D-10/
C---- Electron charge, in ergs/electron Volt
      data ERGPEV                       /1.60217733D-12/
C---- First Bohr radius (cm)
      data BOHRAD                       /5.291775D-9/
C---- Solar diameter (milliarcseconds)
      data SOLDIA                       /1.91926D6/
C
C---- Pi
      data PI                           /3.141592653589793D0/
C---- Root Pi
      data ROOTPI                       /1.772453850905516D0/
C---- Frequency units (Hz)
      data FRQUNT                       /1.D+15/
C---- Mega-barn
      data XMBARN                       /1.D-18/
C---- cm/km
      data CMPKM                        /1.D5/
C---- Angstroms/cm
      data ANGPCM                       /1.D8/
C---- Root 2
      data ROOT2                        /1.414213562373095D0/
C---- Root 3
      data ROOT3                        /1.732050807568877D0/
C---- log10(e)
      data XLOGE                        /4.342944819032518D-1/
C---- degrees / radian
      data DGPRAD                       /5.729577951308232D1/
C---- 1 Debye (statcoulomb cm)
      data DEBYE                        /1.D-18/
C     !EJECT
C
      call HI ('KOSMOS')
C     !BEG
C---- Rydberg wavelength
      call HYDATA (1,dummy1,dummy2,RYDBRG)
C
C---- electron Volts / Kelvin
      EVPK   = BOLZMN/(PLANCK*FREQEV)
C
C---- Stefan-Boltzmann constant
      STFBLZ = (TWO*(PI**5)*(BOLZMN**4))/(F15*(CLIGHT**2)*(PLANCK**3))
C
C---- Photoionization cross-section, head of Hydrogen Lyman continuum
      HYLYK  = (EIGHT*(PLANCK**3))/((THREE*ROOT3)*(PI**2)*(ELMASS**2)
     $         *CLIGHT*(ELCHRG**2))
C     !END
      call BYE ('KOSMOS')
C
      return
      end
