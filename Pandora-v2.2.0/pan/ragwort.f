      subroutine RAGWORT
     $(X,IX,W,IW,XCBL,SWAVE,SLTIT,NSH)
C
C     Rudolf Loeser, 1995 Apr 07
C---- Sets up data for Composite Line Opacity continuum calculations.
C     (This is version 2 of RAGWORT.)
C     !DASH
      save
C     !DASH
      real*8 SLTIT, SWAVE, W, X, XCBL
      integer IW, IWS, IX, JJBNL, JJBNU, JJIBE, JJWVC, JJYWC, JN, JOPAC,
     $        JOPAT, JSTCN, KCOMP, MUX, NCP, NSH, jummy1, jummy2
      logical DOIT, SWTA, SWTB
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(44),NCP)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(157),JJWVC)
      equivalence (IZOQ(159),JJYWC)
      equivalence (IZOQ(160),JJBNL)
      equivalence (IZOQ(161),JJBNU)
C
C---- MINIGER     as of 2006 Jan 12
      integer     JBSCR,JZOQ
      dimension   JZOQ(17)
      common      /MINIGER/ JBSCR,JZOQ
C     INTEGER*4 General Data Block components index.
      equivalence (JZOQ(  7),JJIBE)
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 35),JSTCN)
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
      external AVENS, HARRAN, MORTAIN, IGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XCBL(Miklen), SWAVE(Konwal), SLTIT(Konwal)
      dimension XCBL(*),      SWAVE(*),      SLTIT(*)
C
      dimension JN(2)
      equivalence
     $(JN( 1),JOPAC ),(JN( 2),JOPAT )
C
      call HI ('RAGWORT')
C     !BEG
      call AVENS     (JSTCN, jummy1, KCOMP, jummy2)
C
      SWTA = ((JSTCN.le.0).or.(KCOMP.eq.1)).and.(NCP.gt.0)
      SWTB = (KOPAC(24).gt.0).or.(KOPAC(25).gt.0)
      DOIT = SWTA.and.SWTB
C
      if(DOIT) then
C       (Get, and allocate, IW allotment)
        call MORTAIN (JN, IWS, MUX, 'RAGWORT')
C
        call HARRAN  (X, XCBL, X(JJWVC), X(JJYWC), X(JJBNL), X(JJBNU),
     $                IX(JJIBE), IW(JOPAC), IW(JOPAT), SWAVE, SLTIT,
     $                NSH)
C
C       (Give back IW allotment)
        call IGIVE   (IW, 'RAGWORT')
      end if
C     !END
      call BYE ('RAGWORT')
C
      return
      end
