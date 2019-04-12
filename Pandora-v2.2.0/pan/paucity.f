      subroutine PAUCITY
     $(X,XCBL,XNU,XNUC,XK,YLI,JOPAC,JOPAT)
C
C     Rudolf Loeser, 1975 Dec 09
C---- Sets up Lyman continuum calculations.
C     (This is version 2 of PAUCITY.)
C     !DASH
      save
C     !DASH
      real*8 BMULT, FMULT, R, X, XCBL, XK, XLM, XLTIT, XNU, XNUC, YLI
      integer I, ITYPE, JOPAC, JOPAT, KISLV, KK, KOLEV, KRESN, LIC
      character BLANK*1
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(10),KK )
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
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 33),KOLEV)
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external ANGIE, JUDITH, PICKLE, LIMAN, ZONK, GRID, HI, BYE
C
      dimension X(*)
C
C               XNUC(NSL), XK(KK), YLI(KK), XCBL(Miklen), JOPAC(Nopac),
      dimension XNUC(*),   XK(*),  YLI(*),  XCBL(*),      JOPAC(*),
C
C               JOPAT(Nopac), XNU(NSL)
     $          JOPAT(*),     XNU(*)
C
      data ITYPE,LIC,BMULT /10, 0, 1.D0/
C
      call HI ('PAUCITY')
C     !BEG
      call LIMAN    (1, KRESN, KISLV)
      GENLAB(1) = 'Value of KISLV (=KOLEV)'
      GENLAB(2) = 'Value of KRESN (=contributor index)'
      GENLAB(3) = 'XK-table entry index'
C
      R = XNUC(KOLEV)-XNU(KOLEV)
C
      do 100 I = 1,KK
        call ANGIE  ((R*XK(I)), XLM)
C
        call JUDITH (I, KRESN, KISLV, ITYPE, XLTIT)
        call ZONK   (X, XLM, FMULT, JOPAC, JOPAT, 1)
        call GRID   (XCBL, XLTIT, XLM, YLI(I), FMULT, BMULT, KRESN,
     $               KISLV, JOPAC, JOPAT, NOPAC)
        call PICKLE (XCBL, XLM, XLTIT, ITYPE, LIC)
  100 continue
C     !END
      call BYE ('PAUCITY')
C
      return
      end
