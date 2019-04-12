      subroutine HEGRO
     $(XLCOA,XLCOB,NCB,LCOW)
C
C     Rudolf Loeser, 1987 Nov 16
C---- Fills in the CO-lines opacity data tables.
C     !DASH
      save
C     !DASH
      real*8 XLCOA, XLCOB
      integer LCOW, NCB
      logical DOCO
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
      external FOCA, WEAVE, ALISMA, HI, BYE
C
C               XLCOA(NCB), XLCOB(NCB)
      dimension XLCOA(*),   XLCOB(*)
C
      call HI ('HEGRO')
C     !BEG
C---- Verify that band(s) are specified properly
      call FOCA      (XLCOA, XLCOB, NCB, DOCO)
C
      if(DOCO.and.(KOPAC(27).gt.0)) then
C----   Fill in CO data tables
        call WEAVE
C----   Compute LCOW, the number of CO lines in use
        call ALISMA  (XLCOA, XLCOB, NCB, LCOW)
C
      else
        LCOW = 0
      end if
C     !END
      call BYE ('HEGRO')
C
      return
      end
