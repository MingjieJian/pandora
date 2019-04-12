      subroutine JOLLY
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 2005 Mar 24
C---- Allocates scratch storage for DION.
C     (This is version 3 of JOLLY.)
C     !DASH
      save
C     !DASH
      integer IN, IS, MC, MUX, N
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
C     !DASH
C     !EJECT
      external WGET, WLCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('JOLLY')
C     !BEG
      call WGET (IS,  CALLER)
C
      MC = N*NOPAC
C
      IN( 1) = IS
      IN( 2) = IN( 1)+MC
      IN( 3) = IN( 2)+MC
      IN( 4) = IN( 3)+N
      IN( 5) = IN( 4)+MC
      IN( 6) = IN( 5)+MC
      MUX    = IN( 6)+MC
C
      call WLCK (MUX, CALLER)
C     !END
      call BYE ('JOLLY')
C
      return
      end
