      subroutine FLOOR
     $(NO)
C
C     Rudolf Loeser, 1983 Feb 10
C---- Prints universal constants.
C     !DASH
      save
C     !DASH
      real*8 AUNIT, BOHRAD, BOLZMN, CLIGHT, DGPRAD, ELCHRG, ELMASS, F10,
     $       F11, F12, F18, F2, F3, F8, FA, FREQEV, FX, FY, HEMASS,
     $       HYMASS, PLANCK, RYDBRG, SOLDIA, SOLRAD, SOLSGR, T, TEN,
     $       dummy
      integer I, IPEX, IQSTA, LUEO, NF, NO
      character FIELDS*30
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 18),IPEX )
C
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
      equivalence (DLIT(11),TEN   )
C     !EJECT
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ( 76),IQSTA)
C     !DASH
      external PRIAM, LINER, ENCODED, MESHED, DVECOUT, RIGEL, MASHED,
     $         HI, BYE
C
      dimension T(14), FIELDS(14)
C
      data F2, F3, F8, F10, F11 /1.D2, 1.D3, 1.D8, 1.D10, 1.D11/
      data F12, F18 /1.D12, 1.D18/
      data FA /3.6D3/
C     !EJECT
C
      call HI ('FLOOR')
C     !BEG
      if(IQSTA.gt.0) then
        call PRIAM   (NO, 'CONSTANTS', 9)
C
        T( 1) = PLANCK
        T( 2) = BOLZMN
        T( 3) = CLIGHT
        T( 4) = RYDBRG
        T( 5) = FREQEV
        T( 6) = ELCHRG
        T( 7) = ELMASS
        T( 8) = HYMASS
        T( 9) = BOHRAD
        T(10) = HEMASS
        T(11) = SOLSGR
        T(12) = SOLRAD
        T(13) = SOLDIA
        T(14) = AUNIT
C
        FX = CLIGHT/(TEN*F18)
        FY = FA*DGPRAD
C
        call ENCODED (T( 1)    , FIELDS( 1), 30, 10, 1, NF)
        call ENCODED (T( 2)*F18, FIELDS( 2), 30, 10, 1, NF)
        call ENCODED (T( 3)/F8 , FIELDS( 3), 30, 10, 1, NF)
        call ENCODED (T( 4)/TEN, FIELDS( 4), 30, 10, 1, NF)
        call ENCODED (T( 5)/F12, FIELDS( 5), 30, 10, 1, NF)
        call ENCODED (T( 6)/FX , FIELDS( 6), 30, 10, 1, NF)
        call ENCODED (T( 7)/F3 , FIELDS( 7), 30, 10, 1, NF)
        call ENCODED (T( 8)/F3 , FIELDS( 8), 30, 10, 1, NF)
        call ENCODED (T( 9)*F10, FIELDS( 9), 30, 10, 1, NF)
        call ENCODED (T(10)    , FIELDS(10), 30, 10, 1, NF)
        call ENCODED (T(11)/F2 , FIELDS(11), 30, 10, 1, NF)
        call ENCODED (T(12)/F8 , FIELDS(12), 30, 10, 1, NF)
        call ENCODED (T(13)/FY , FIELDS(13), 30, 10, 1, NF)
        call ENCODED (T(14)/F11, FIELDS(14), 30, 10, 1, NF)
C     !EJECT
        call LINER (3, NO)
        write (NO,100) (FIELDS(I),T(I),I=1,8)
  100   format(' ','The following values of physical constants ',
     $             'have been built in:',1P//
     $         ' ',A30,' (erg s)',6X,E24.12,' (erg s)',6X,
     $            'Planck''s constant'//
     $         ' ',A30,' (atto-erg/K)',1X,E24.12,' (erg/K)',6X,
     $            'Boltzmann''s constant'//
     $         ' ',A30,' (mega-m/s)',3X,E24.12,' (cm/s)',7X,
     $            'velocity of light'//
     $         ' ',A30,' (nano-m)',5X,E24.12,' (Angstrom)',3X,
     $            'Rydberg wavelength (derived)'//
     $         ' ',A30,' (tera-Hz)',4X,E24.12,' (Hz)',9X,
     $            'frequency for 1 ev'//
     $         ' ',A30,' (atto-C)',5X,E24.12,' (esu)',8X,
     $            'electron charge'//
     $         ' ',A30,' (kilo-g)',5X,E24.12,' (g)',10X,
     $            'electron mass'//
     $         ' ',A30,' (kilo-g)',5X,E24.12,' (g)',10X,
     $            'Hydrogen atom mass')
        call LINER (1, NO)
        write (NO,101) (FIELDS(I),T(I),I=9,14)
  101   format(1P,
     $         ' ',A30,' (pico-m)',5X,E24.12,' (cm)',9X,
     $            'first Bohr radius'//
     $         ' ',A30,'   ',11X,E24.12,'   ',11X,
     $            'Helium/Hydrogen mass ratio'//
     $         ' ',A30,' (m/s**2)',5X,E24.12,' (cm/s**2)',4X,
     $            'solar surface gravity'//
     $         ' ',A30,' (mega-m)',5X,E24.12,' (cm)',9X,
     $            'solar radius'//
     $         ' ',A30,' (milli-rad)',2X,E24.12,' (mas)',8X,
     $            'solar angular diameter'//
     $         ' ',A30,' (giga-m)',5X,E24.12,' (cm)',9X,
     $            'Astronomical Unit')
C     !EJECT
        call LINER (8, NO)
        write (NO,102)
  102   format(' ','N.B:',5X,'yocto',2X,'zepto',2X,'atto',3X,'femto',
     $             2X,'pico',3X,'nano',3X,'micro',2X,'milli',7X,'kilo',
     $             3X,'mega',3X,'giga',3X,'tera',3X,'peta',3X,'exa',
     $             4X,'zetta',2X,'yotta'/
     $         ' ',9X,'-24',4X,'-21',4X,'-18',4X,'-15',4X,'-12',4X,'-9',
     $             5X,'-6',5X,'-3',10X,'+3',5X,'+6',5X,'+9',5X,'+12',4X,
     $             '+15',4X,'+18',4X,'+21',4X,'+24'/
     $         ' ',10X,'y',6X,'z',6X,'a',6X,'f',6X,'p',6X,'n',5X,'mu',
     $             6X,'m',11X,'k',6X,'M',6X,'G',6X,'T',6X,'P',6X,'E',
     $             6X,'Z',6X,'Y')
      end if
      if((IPEX.lt.0).or.(IPEX.eq.21)) then
        call MESHED  ('FLOOR', 3)
        write (LUEO,103)
  103   format(' ','Preset and precomputed constants.')
        call DVECOUT (LUEO, PCON, MCONSH, 'Preset physical constants')
        call DVECOUT (LUEO, TUNI, MUNISH, 'Preset other constants'   )
        call LINER   (1, LUEO)
        call RIGEL   (-LUEO, dummy)
        call MASHED  ('FLOOR')
      end if
C     !END
      call BYE ('FLOOR')
C
      return
      end
