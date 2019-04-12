      subroutine JIM
C
C     Rudolf Loeser, 1983 Jun 01
C---- Opens auxiliary input files.
C     (This is version 2 of JIM.)
C     !DASH
      save
C     !DASH
      integer JAYTI, JNUNC, KURU, LUEO, NOION
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
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 34),JNUNC)
      equivalence (KZQ( 94),NOION)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(22),JAYTI)
      equivalence (LUNITS(17),KURU )
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external LUCK, HI, BYE
C
      call HI ('JIM')
C     !BEG
      if((JNUNC.gt.0).and.(NOION.le.0)) then
C----   P.R.D. restart Jnu input file
        call LUCK (JAYTI, LUEO)
      end if
C
      if((KOPAC(22)+KOPAC(23)).gt.0) then
C----   Statistical line opacity data file
        call LUCK (KURU, LUEO)
      end if
C     !END
      call BYE ('JIM')
C
      return
      end
