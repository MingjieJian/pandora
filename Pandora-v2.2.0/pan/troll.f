      subroutine TROLL
     $(X,XCBL,STAB,NS,YRATS,JOPAC,JOPAT)
C
C     Rudolf Loeser, 2002 Sep 26
C---- Sets up Continuum Data Blocks for standard rates integrations
C     wavelengths, for HOVEL.
C     !DASH
      save
C     !DASH
      real*8 BMULT, FMULT, STAB, X, XCBL, XLM, XLTIT, YRATS
      integer I, ITYPE, JOPAC, JOPAT, KISLV, KRESN, LIC, NS
      character BLANK*1
C     !COM
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
C---- SALKA       as of 1989 Nov 17
      character   GENLAB*60
      dimension   GENLAB(4)
      common      /SALKA/ GENLAB
C     Texts for error messages from GENISTA/AJUGA.
C     .
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
C     !EJECT
      external JUDITH, PICKLE, ZONK, GRID, LIMAN, HI, BYE
C
      dimension X(*)
C
C               XCBL(Miklen), STAB(NS), JOPAC(Nopac), JOPAT(Nopac)
      dimension XCBL(*),      STAB(*),  JOPAC(*),     JOPAT(*)
C
      data ITYPE,LIC,BMULT /27, 0, 1.D0/
C
      call HI ('TROLL')
C     !BEG
      GENLAB(1) = 'Standard rates integration table'
      GENLAB(2) = 'Wavelength index'
      GENLAB(3) = BLANK
C
      call LIMAN    (0, KRESN, KISLV)
C
      do 100 I = 1,NS
        XLM = STAB(I)
C
        call JUDITH (I, 0, 0, ITYPE, XLTIT)
        call ZONK   (X, XLM, FMULT, JOPAC, JOPAT, 1)
        call GRID   (XCBL, XLTIT, XLM, YRATS, FMULT, BMULT, KRESN,
     $               KISLV, JOPAC, JOPAT, NOPAC)
        call PICKLE (XCBL, XLM, XLTIT, ITYPE, LIC)
  100 continue
C     !END
      call BYE ('TROLL')
C
      return
      end
