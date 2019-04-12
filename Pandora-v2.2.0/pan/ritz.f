      subroutine RITZ
     $(LU)
C
C     Rudolf Loeser, 1974 Jan 07
C---- Prints end-of-run external file data.
C     !DASH
      save
C     !DASH
      integer ITOPE, KURIN, LU, NAB
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(45),NAB)
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
C     !EJECT
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 27),KURIN)
      equivalence (KZQ( 17),ITOPE)
C     !DASH
      external LINER, HI, BYE
C
      call HI ('RITZ')
C     !BEG
      if(((KURIN.ge.1).and.(KURIN.le.10)).and.
     $   ((KOPAC(22).gt.0).or.(KOPAC(23).gt.0))) then
        call LINER (2, LU)
        write (LU,100)
  100   format(' ','Statistical Line Opacity (KURUCZ) data was read')
      end if
C
      if((NAB.gt.0).and.
     $   ((KOPAC(24).gt.0).or.(KOPAC(25).gt.0))) then
        call LINER (2, LU)
        write (LU,101)
  101   format(' ','Composite Line Opacity (KURUCZ) data was read')
      end if
C
      if(ITOPE.gt.0) then
        call LINER (2, LU)
        write (LU,102)
  102   format(' ','Continuum Plot data was saved')
      end if
C     !END
      call BYE ('RITZ')
C
      return
      end
