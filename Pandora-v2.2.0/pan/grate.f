      subroutine GRATE
     $(X,XCBL,JLEV,NSL,ITYPE,WAV,RCP,YR,MRJ,WRATIJ,RRCPIJ,YRATE,
     $ JOPAC,JOPAT,SWAVE,SLTIT,NSH)
C
C     Rudolf Loeser, 1980 Mar 04
C---- Sets up Continuum Data Blocks for rates integrations,
C     for level JLEV, for HOTEL.
C     (This is version 2 of GRATE.)
C     !DASH
      save
C     !DASH
      real*8 BMULT, DAMP, FMULT, RCP, RRCPIJ, SLTIT, SWAVE, WAV, WRATIJ,
     $       X, XCBL, XLM, XLTIT, YR, YRATE
      integer I, ITYPE, JLEV, JOPAC, JOPAT, KISLV, KRESN, LIC, MR, MRJ,
     $        MRP, NSH, NSL
      logical EXISTS
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
C     !DASH
C     !EJECT
      external MARTHA, JUDITH, PICKLE, GHERKIN, LIMAN, PAPAYA, GRID,
     $         ZONK, HI, BYE
C
      dimension X(*)
C
C               MRZ = MRS+NSL+1
C
C               SWAVE(Konwal), SLTIT(Konwal), XCBL(Miklen), YRATE(MRZ),
      dimension SWAVE(*),      SLTIT(*),      XCBL(*),      YRATE(*),
C
C               WAV(MRX), YR(MRX), RCP(MRX), WRATIJ(MRZ), RRCPIJ(MRZ),
     $          WAV(*),   YR(*),   RCP(*),   WRATIJ(*),   RRCPIJ(*),
C
C               MRJ(NSL+1), JOPAC(Nopac), JOPAT(Nopac)
     $          MRJ(*),     JOPAC(*),     JOPAT(*)
C
      data LIC,BMULT /0, 1.D0/
C
      call HI ('GRATE')
C     !BEG
      GENLAB(1) = 'Rates-integration wavelength index'
      GENLAB(2) = 'Ion-of-the-run level index'
      GENLAB(3) = ' '
C
      call LIMAN       (0, KRESN, KISLV)
      call PAPAYA      (JLEV, WAV, RCP, YR, MR, MRP, MRJ, WRATIJ,
     $                  RRCPIJ, YRATE)
C
      do 100 I = 1,MRP
        XLM  = WAV(I)
        DAMP = YR(I)
C
        call JUDITH    (I, JLEV, 0, ITYPE, XLTIT)
        call MARTHA    (XLM, ITYPE, EXISTS)
C
        if(.not.EXISTS) then
          call ZONK    (X, XLM, FMULT, JOPAC, JOPAT, 1)
          call GRID    (XCBL, XLTIT, XLM, DAMP, FMULT, BMULT, KRESN,
     $                  KISLV, JOPAC, JOPAT, NOPAC)
          call PICKLE  (XCBL, XLM, XLTIT, ITYPE, LIC)
        else
          call GHERKIN (SWAVE, SLTIT, NSH, XLM, XLTIT, 'GRATE')
        end if
C
  100 continue
C     !END
      call BYE ('GRATE')
C
      return
      end
