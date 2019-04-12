      subroutine BUSTER
     $(X,IX,W,IW,DDL,CDL,CRD,CVW,CSK,CRS)
C
C     Rudolf Loeser, 2004 Aug 17
C---- Checks the data needed for background contributor lines.
C     (This is version 4 of BUSTER.)
C     !DASH
      save
C     !DASH
      real*8 AMASS, CDL, CRD, CRS, CSK, CVW, DDL, W, X
      integer IONST, IW, IX, JJAIJ, JJP, JJXNU, KEEOK, KHEOK, KOXOK,
     $        KX2OK, KX3OK
      character QELSM*8
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
C     !EJECT
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 32),JJAIJ)
      equivalence (IZOQ( 27),JJP  )
      equivalence (IZOQ( 26),JJXNU)
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
      equivalence (QZQ(  2),QELSM)
      equivalence (KZQ( 56),IONST)
      equivalence (RZQ(  4),AMASS)
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
      equivalence (LEST(73),KHEOK)
      equivalence (LEST(72),KOXOK)
      equivalence (LEST(77),KEEOK)
      equivalence (LEST(57),KX2OK)
      equivalence (LEST(58),KX3OK)
C     !DASH
C     !EJECT
      external DOTTY, HOTTY, MOTTY, POTTY, LOTTY, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               CRD(LDLMX,NT), CVW(LDLMX,NT), CSK(LDLMX,NT)
      dimension CRD(*),        CVW(*),        CSK(*),
C
C               DDL(LDLMX,NT), CDL(LDLMX,NT), CRS(LDLMX,NT)
     $          DDL(*),        CDL(*),        CRS(*)
C
      call HI ('BUSTER')
C     !BEG
      if(KOPAC(35).gt.0) then
        if((QELSM(1:2).eq.'HE').and.(IONST.eq.1)) then
C----     Check whether He-I data sets agree
          call MOTTY (AMASS, X(JJAIJ), X(JJP), X(JJXNU), CRD, CVW,
     $                CSK, KEEOK)
        end if
      end if
C
      if(KOPAC(33).gt.0) then
        if((QELSM(1:2).eq.'HE').and.(IONST.eq.2)) then
C----     Check whether He-II data sets agree
          call HOTTY (AMASS, X(JJAIJ), X(JJP), X(JJXNU), CRD, CVW,
     $                CSK, DDL, CDL, KHEOK)
        end if
      end if
C
      if(KOPAC(39).gt.0) then
        if((QELSM(1:2).eq.'O ').and.(IONST.eq.1)) then
C----     Check whether O-I data sets agree
          call DOTTY (AMASS, X(JJAIJ), X(JJP), X(JJXNU), CRD, CVW,
     $                CSK, KOXOK)
        end if
      end if
C
      if(KOPAC(44).gt.0) then
        if((QELSM(1:2).eq.'O ').and.(IONST.eq.2)) then
C----     Check whether O-II data sets agree
          call POTTY (AMASS, X(JJAIJ), X(JJP), X(JJXNU), CRD, CVW,
     $                CSK, DDL, CDL, KX2OK)
        end if
      end if
C
      if(KOPAC(45).gt.0) then
        if((QELSM(1:2).eq.'O ').and.(IONST.eq.3)) then
C----     Check whether O-III data sets agree
          call LOTTY (AMASS, X(JJAIJ), X(JJP), X(JJXNU), CRD, CVW,
     $                CSK, DDL, CDL, KX3OK)
        end if
      end if
C     !END
      call BYE ('BUSTER')
C
      return
      end
