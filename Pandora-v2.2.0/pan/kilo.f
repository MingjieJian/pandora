      subroutine KILO
     $(X,XCBL,AWAVE,YWAVE,JOPAC,JOPAT,SWAVE,SLTIT,NSH)
C
C     Rudolf Loeser, 1975 Dec 09
C---- Sets up "additional" continuum calculations.
C     (This is version 2 of KILO.)
C     !DASH
      save
C     !DASH
      real*8 AWAVE, BMULT, DAMP, FMULT, SLTIT, SWAVE, X, XCBL, XLM,
     $       XLTIT, YWAVE
      integer I, ITYPE, JOPAC, JOPAT, KISLV, KRESN, LIC, NSH, NWV
      logical EXISTS
      character BLANK*1
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(17),NWV)
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
C---- SALKA       as of 1989 Nov 17
      character   GENLAB*60
      dimension   GENLAB(4)
      common      /SALKA/ GENLAB
C     Texts for error messages from GENISTA/AJUGA.
C     .
C     !EJECT
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  MARTHA, JUDITH, PICKLE, LIMAN, GHERKIN, GRID, JANOS,
     $          ZONK, HI, BYE
      intrinsic abs
C
      dimension X(*)
C
C               YWAVE(NWV), XCBL(Miklen), SWAVE(Konwal), SLTIT(Konwal),
      dimension YWAVE(*),   XCBL(*),      SWAVE(*),      SLTIT(*),
C
C               AWAVE(NWV), JOPAC(Nopac), JOPAT(Nopac)
     $          AWAVE(*),   JOPAC(*),     JOPAT(*)
C
      data LIC,BMULT /0, 1.D0/
C
      call HI ('KILO')
C     !BEG
      GENLAB(1) = 'WAVES-table entry index'
      GENLAB(2) = BLANK
      GENLAB(3) = BLANK
C
      call LIMAN       (0, KRESN, KISLV)
      do 100 I = 1,NWV
C
        XLM  = abs(AWAVE(I))
        DAMP = YWAVE(I)
        call JANOS     (AWAVE(I), ITYPE)
        call JUDITH    (I, 0, 0, ITYPE, XLTIT)
        call MARTHA    (XLM, ITYPE, EXISTS)
C
        if(.not.EXISTS) then
          call ZONK    (X, XLM, FMULT, JOPAC, JOPAT, 1)
          call GRID    (XCBL, XLTIT, XLM, DAMP, FMULT, BMULT, KRESN,
     $                  KISLV, JOPAC, JOPAT, NOPAC)
          call PICKLE  (XCBL, XLM, XLTIT, ITYPE, LIC)
        else
          call GHERKIN (SWAVE, SLTIT, NSH, XLM, XLTIT, 'KILO')
        end if
C
  100   continue
C     !END
      call BYE ('KILO')
C
      return
      end
