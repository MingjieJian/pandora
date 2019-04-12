      subroutine MERANIA
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1995 Aug 09
C---- Allocates integer scratch storage for DUGOUT.
C     !DASH
      save
C     !DASH
      integer IN, IS, LCOW, LNGTH, MUX, NCB, NCL
      character CALLER*(*)
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
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(51),NCL)
      equivalence (JZQ(55),NCB)
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(44),LCOW )
C     !DASH
C     !EJECT
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('MERANIA')
C     !BEG
      call IGET (IS,  CALLER)
C
      LNGTH = LCOW*(2*NCL-1)+2*NCB
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+LNGTH
      IN( 3) = IN( 2)+LNGTH
      IN( 4) = IN( 3)+LNGTH
      IN( 5) = IN( 4)+LNGTH
      IN( 6) = IN( 5)+NOPAC
      MUX    = IN( 6)+NOPAC
C
      call ILCK (MUX, CALLER)
C     !END
      call BYE ('MERANIA')
C
      return
      end
