      subroutine TRUDE
     $(X,XCBL,NW,WAVES,CORE,DAMP,ITYPE,KPRD,OML,KODE,JOPAC,JOPAT)
C
C     Rudolf Loeser, 1977 Sep 13
C---- Sets up Continuum Data Blocks for line background calculations.
C     (This is version 2 of TRUDE.)
C     !DASH
      save
C     !DASH
      real*8 CORE, DAMP, FMULT, OML, WAVES, X, XCBL, XLM, XLTIT
      integer I, IL, ITYPE, IU, JOPAC, JOPAT, JTYPE, K1, KISLV, KLIN,
     $        KODE, KPRD, KRESN, LIC, LOML, NW
      logical TYPE
C     !COM
C---- SALKA       as of 1989 Nov 17
      character   GENLAB*60
      dimension   GENLAB(4)
      common      /SALKA/ GENLAB
C     Texts for error messages from GENISTA/AJUGA.
C     .
C---- LINUS       as of 2004 May 12
      integer     LINKDS
      dimension   LINKDS(22)
      common      /LINUS/ LINKDS
C     Line source function calculation control parameters for the
C     current transition as set up by "PET" (and printed by "LINSEED").
C     IU    - index of upper level
C     IL    - index of lower level
C     KLIN  - line "type" code (1: radiative, 2: passive, etc)
C     ICE   - PRD calculation control
C     IPRO  - emergent profiles calculation control
C     METSE - statistical equilibrium calculation method selector
C     METSF - LSF calculation method selector (QR, RT, GR)
C     IBRSW - damping components selector
C     INKSW - input opacity signal
C     LSFT  - LSF solution code (0: full, 1:direct, etc)
C     ILFLX - line flux calculation control
C     LDL   - number of line components
C     LINT  - frequency integration range (half vs. full profile)
C     LSFP  - LSF printout control
C     IFDB  - LSF background control (constant vs. varying)
C     ISBG  - blended line profile plot mode switch
C     KBT   - length of input table XIBLUT
C     KRT   - length of input table XIREDT
C     KST   - length of input table XISYMT
C     KTRN  - length of actual tables XI and DL
C     LOML  - "line-background-continuum-opacity" control
C     ....  - (available)
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
      equivalence (LINKDS( 3),KLIN )
      equivalence (LINKDS(21),LOML )
C     !EJECT
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
      external PICKLE, DROMOND, SUNDEW, MELANIA, JUDITH, MOVEI, JAIROU,
     $         LIMAN, TINTUD, ORACHE, GRID, HI, BYE
C
      dimension X(*)
C
C               WAVES(NW), XCBL(Miklen), JOPAT(Nopac), JOPAC(Nopac)
      dimension WAVES(*),  XCBL(*),      JOPAT(*),     JOPAC(*)
C
      data FMULT /1.D0/
C
      call HI ('TRUDE')
C     !BEG
      LIC  = 100*IU+IL
      TYPE = KODE.ge.0
C
      GENLAB(1)   = 'Ion-of-the-run level index'
      GENLAB(2)   = 'Ion-of-the-run level index'
      if(TYPE) then
        GENLAB(3) = 'Line-type index'
      else
        GENLAB(3) = 'XI-table entry index'
      end if
C
      call LIMAN       (0, KRESN, KISLV)
      do 100 I = 1,NW
C
        XLM = WAVES(I)
C
        if(TYPE) then
          K1 = KLIN
        else
          K1 = I
        end if
        JTYPE = ITYPE
        call TINTUD    (XLM, CORE, JTYPE)
        call JUDITH    (K1, IU, IL, JTYPE, XLTIT)
C
        call MOVEI     (KOPAC, 1, NOPAC, JOPAC, 1, NOPAC)
        if(LOML.eq.0) then
          call JAIROU  (JOPAC)
        else
          call MELANIA (X, XLM, JOPAC)
        end if
        call DROMOND   (X, XLM, JOPAC)
        call SUNDEW    (JOPAC, JOPAT, NOPAC)
C
        call GRID      (XCBL ,XLTIT, XLM, DAMP, FMULT, OML, KRESN,
     $                  KISLV, JOPAC, JOPAT, NOPAC)
        call ORACHE    (XCBL, KPRD)
        call PICKLE    (XCBL, XLM, XLTIT, JTYPE, LIC)
C
  100 continue
C     !END
      call BYE ('TRUDE')
C
      return
      end
