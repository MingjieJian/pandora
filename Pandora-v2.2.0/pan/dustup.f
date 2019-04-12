      subroutine DUSTUP
     $(X,WVLO,WVL,WVHI,CRIT,CDW,DNU,DUMP,XIA,K,YESH,YESO,YEES,YESE,
     $ YES2,YES3)
C
C     Rudolf Loeser, 2004 Apr 23
C---- Assembles a complete set of potential additional XI values needed
C     to capture adequately any b-f absorption edge or background line
C     falling within the wavelength range (WVLO--WVHI) spanned by the
C     basic XI table of the current transition.
C     !DASH
      save
C     !DASH
      real*8 BAND, CDW, CRIT, DNU, TEN, WVHI, WVL, WVLO, X, XIA
      integer K, KM, LIM, LYM
      logical DUMP, YEES, YES2, YES3, YESE, YESH, YESO
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(49),KM )
C
C---- OPACITY     as of 2007 Jan 12
C     Paraphernalia for background absorption/emission contributors.
C
C     (Must recompile BARE, BRACE, FORAGER & SHARI when changing NABS!)
      parameter   (NABS=45)
C
      integer     NABS,NOPAC,KOPAC,TOPAC,LOPAC
      character   CNAME*24,SYMID*1,SHNAM*6
      dimension   KOPAC(NABS),LOPAC(NABS),SYMID(NABS)
      dimension   CNAME(NABS),SHNAM(NABS)
C
      common      /OPAC1/ NOPAC
      common      /OPAC2/ KOPAC
      common      /OPAC3/ LOPAC
      common      /OPAC4/ CNAME
      common      /OPAC5/ SYMID
      common      /OPAC6/ SHNAM
C
C     NOPAC = number of contributors
C     KOPAC = contributor status switch: 0 = don't use, 1 = use
C     CNAME = name (description) of contributor
C             NOTE: This sequence of names establishes the indices or
C                   ordinals by which the contributors are also known.
C     SHNAM = "short" name of contributor
C     LOPAC = "printout order" list of contributor ordinals
C     SYMID = scratch space for symbolic identifiers
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(11),TEN   )
C     !DASH
C     !EJECT
      external TRIVIAL, ADDLYM, ADDOXY, ADDEEL, ADDELI, MESHED, MASHED,
     $         TUNE, XIAN, ADDOX2, ADDOX3, HI, BYE
C
      dimension X(*)
C
C               XIA(KM)
      dimension XIA(*)
C
      call HI ('DUSTUP')
C     !BEG
      BAND = CRIT*TEN
C
      K = 0
C---- Bound-free absorption edges from population-ion tables
      call TRIVIAL  (KM, XIA, K, WVLO, WVHI, BAND)
C
      if(DUMP) then
        call MESHED ('DUSTUP', 2)
      end if
      call TUNE     (LYM)
      if(LYM.gt.1) then
C----   Hydrogen Lyman line wavelengths to "XIA"
        LIM = LYM-1
        call ADDLYM (X, LIM, WVLO, WVL, WVHI, CRIT, DUMP, XIA, K, YESH)
      end if
      if(KOPAC(39).gt.0) then
C----   Oxygen-I line wavelengths to "XIA"
        call ADDOXY (X,      WVLO, WVL, WVHI, CRIT, DUMP, XIA, K, YESO)
      end if
      if(KOPAC(44).gt.0) then
C----   Oxygen-II line wavelengths to "XIA"
        call ADDOX2 (X,      WVLO, WVL, WVHI, CRIT, DUMP, XIA, K, YES2)
      end if
      if(KOPAC(45).gt.0) then
C----   Oxygen-III line wavelengths to "XIA"
        call ADDOX3 (X,      WVLO, WVL, WVHI, CRIT, DUMP, XIA, K, YES3)
      end if
      if(KOPAC(35).gt.0) then
C----   Helium-I line wavelengths to "XIA"
        call ADDEEL (X,      WVLO, WVL, WVHI, CRIT, DUMP, XIA, K, YEES)
      end if
      if(KOPAC(33).gt.0) then
C----   Helium-II line wavelengths to "XIA"
        call ADDELI (X,      WVLO, WVL, WVHI, CRIT, DUMP, XIA, K, YESE)
      end if
      if(DUMP) then
        call MASHED ('DUSTUP')
      end if
C
      if(K.gt.0) then
C----   Convert wavelengths in "XIA" to XI-values, XIA
        call XIAN   (WVL, DNU, CDW, XIA, K)
      end if
C     !END
      call BYE ('DUSTUP')
C
      return
      end
