      subroutine SABLE
     $(STATST,COMPOS,AVERAG,LINEOP)
C
C     Rudolf Loeser, 1985 Dec 18
C---- Computes processing switches for Kurucz opacities.
C     !DASH
      save
C     !DASH
      logical AVERAG, COMPOS, LINEOP, STATST
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
      external HI, BYE
C
      call HI ('SABLE')
C     !BEG
      STATST = ((KOPAC(22)+KOPAC(23)).gt.0)
      COMPOS = ((KOPAC(24)+KOPAC(25)).gt.0)
      AVERAG = ((KOPAC(31)+KOPAC(32)).gt.0)
C
      LINEOP = (STATST.or.COMPOS.or.AVERAG)
C     !END
      call BYE ('SABLE')
C
      return
      end
