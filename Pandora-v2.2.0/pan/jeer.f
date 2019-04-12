      subroutine JEER
     $(IN,IS,MUX,CALLER)
C
C     Rudolf Loeser, 1995 Jul 27
C---- Allocates integer scratch storage for BUSY.
C     !DASH
      save
C     !DASH
      integer IN, IS, MUX, NL, NT
      character CALLER*(*)
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 5),NT )
      equivalence (JZQ( 2),NL )
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
      external IGET, ILCK, HI, BYE
C
      dimension IN(*)
C
      call HI ('JEER')
C     !BEG
      call IGET (IS,  CALLER)
C
      IN( 1) = IS
C
      IN( 2) = IN( 1)+50
      IN( 3) = IN( 2)+(2*NL)
      IN( 4) = IN( 3)+NOPAC
      MUX    = IN( 4)+(2*NT)
C
      call ILCK (MUX, CALLER)
C     !END
      call BYE ('JEER')
C
      return
      end
