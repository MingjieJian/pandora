      subroutine UMIAK
     $(X,W,IW,XCBL,XLCOA,XLCOB,WAVES,JIND,KIND,NIND,MIND,JOPAC,JOPAT,
     $ SWAVE,SLTIT,NSH)
C
C     Rudolf Loeser, 1987 Nov 18
C---- Sets up CO-lines continuum calculations.
C
C---- P R E S U M E S   that XCOL(1)=0 when NCL=1.
C     !DASH
      save
C     !DASH
      real*8 BMULT, FMULT, SLTIT, SWAVE, W, WAVES, X, XCBL, XLCOA,
     $       XLCOB, XLM, XLTIT, YCOL
      integer I, ITYPE, IW, JIND, JOPAC, JOPAT, KIND, KISLV, KRESN, LIC,
     $        MIND, NCB, NIND, NSH, NWV
      logical EXISTS, USE
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
C     !EJECT
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(55),NCB)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 97),YCOL )
C
C---- SALKA       as of 1989 Nov 17
      character   GENLAB*60
      dimension   GENLAB(4)
      common      /SALKA/ GENLAB
C     Texts for error messages from GENISTA/AJUGA.
C     .
C     !DASH
C     !EJECT
      external HERBERT, JUDITH, PICKLE, KOSHER, GHERKIN, LIMAN, MARTHA,
     $         ZONK, GRID, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C     The upper limit of NWV is LCOW*(2*NCL-1)+2*NCB, where
C     LCOW is the number of actual CO line cores included.
C
C               WAVES(NWV), JIND(NWV), KIND(NWV), MIND(NWV), NIND(NWV),
      dimension WAVES(*),   JIND(*),   KIND(*),   MIND(*),   NIND(*),
C
C               XLCOB(NCB), SWAVE(Konwal), SLTIT(Konwal), XCBL(Miklen),
     $          XLCOB(*),   SWAVE(*),      SLTIT(*),      XCBL(*),
C
C               XLCOA(NCB), JOPAC(Nopac), JOPAT(Nopac)
     $          XLCOA(*),   JOPAC(*),     JOPAT(*)
C
      data LIC,BMULT /0, 1.D0/
C
      call HI ('UMIAK')
C     !BEG
C---- Get table of wavelengths
      call HERBERT         (X, W, IW, XLCOA, XLCOB, NCB, WAVES, JIND,
     $                      KIND, NIND, MIND, NWV, 1)
      if(NWV.gt.0) then
C
C----   Set up data blocks
        GENLAB(1) = 'Quantum number j'
        GENLAB(2) = 'Quantum number k'
        GENLAB(3) = 'XCL-table entry index'
C
        call LIMAN         (0, KRESN, KISLV)
        do 100 I = 1,NWV
          XLM = WAVES(I)
          call KOSHER      (XLM, X, USE)
C
          if(USE) then
            ITYPE = MIND(I)
            call JUDITH    (JIND(I), KIND(I), NIND(I), ITYPE, XLTIT)
            call MARTHA    (XLM, ITYPE, EXISTS)
C
            if(.not.EXISTS) then
              call ZONK    (X, XLM, FMULT, JOPAC, JOPAT, 0)
              call GRID    (XCBL, XLTIT, XLM, YCOL, FMULT, BMULT, KRESN,
     $                      KISLV, JOPAC, JOPAT, NOPAC)
              call PICKLE  (XCBL, XLM, XLTIT, ITYPE, LIC)
            else
              call GHERKIN (SWAVE, SLTIT, NSH, XLM, XLTIT, 'UMIAK')
            end if
C
          end if
  100   continue
      end if
C     !END
      call BYE ('UMIAK')
C
      return
      end
