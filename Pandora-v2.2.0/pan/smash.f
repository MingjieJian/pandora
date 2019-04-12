      subroutine SMASH
     $(X,NTMX,TAB,NT,WLO,WHI)
C
C     Rudolf Loeser, 2007 Jan 22
C---- Enters a set of wavelengths to provide adequately for the
C     background O-III lines, for TRAVIS.
C     !DASH
      save
C     !DASH
      real*8 DS, TAB, WHI, WLO, X, ZERO
      integer I, K, NT, NTMX
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
C---- NOTHER      as of 2004 Jun 25
      integer     NIAUGM
      parameter   (NIAUGM=3000)
C     (Be sure to recompile all users when changing NIAUGM ! )
C     Upper limit for total profile data points for coincident
C     background lines.
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external STRIVA, LOUDER, HI, BYE
C
      dimension X(*)
C
C               TAB(NTMX)
      dimension TAB(*)
C
      dimension DS(NIAUGM)
C
      call HI ('SMASH')
C     !BEG
      if(KOPAC(45).gt.0) then
        K = 0
        call LOUDER   (X, 1, 0, NIAUGM, DS, K)
        do 100 I = 1,K
          call STRIVA (DS(I), ZERO, WLO, WHI, NTMX, TAB, NT, 'SMASH')
  100   continue
      end if
C     !END
      call BYE ('SMASH')
C
      return
      end
