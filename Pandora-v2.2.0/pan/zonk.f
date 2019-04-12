      subroutine ZONK
     $(X,XLM,FMULT,JOPAC,JOPAT,KODE)
C
C     Rudolf Loeser, 2002 Aug 21
C---- Initializes some parameters in Continuum Data Blocks.
C     !DASH
      save
C     !DASH
      real*8 FMULT, X, XLM
      integer JOPAC, JOPAT, KODE
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
C     !DASH
C     !EJECT
      external MOVEI, MELANIA, DROMOND, SUNDEW, ZEMRUDE, HI, BYE
C
      dimension X(*)
C
C               JOPAC(Nopac), JOPAT(Nopac)
      dimension JOPAC(*),     JOPAT(*)
C
      call HI ('ZONK')
C     !BEG
      call MOVEI     (KOPAC, 1, NOPAC, JOPAC, 1, NOPAC)
C
      call MELANIA   (X, XLM, JOPAC)
      if(KODE.eq.1) then
        call DROMOND (X, XLM, JOPAC)
      end if
C
      call SUNDEW    (JOPAC, JOPAT, NOPAC)
      call ZEMRUDE   (X, XLM, FMULT)
C     !END
      call BYE ('ZONK')
C
      return
      end
