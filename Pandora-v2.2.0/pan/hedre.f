      subroutine HEDRE
C
C     Rudolf Loeser, 1980 Feb 14
C---- Edits KOPAC, the array of absorber switches.
C     !DASH
      save
C     !DASH
      real*8 AB, XKDST, ZERO, dummy
      integer IQALO, K04, K16, KURIN, KWA, LCOW, LODE, LUEO, NAB, NCOSW,
     $        NCP, NHTSW, NLY
      character VAL*3
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(44),NCP)
      equivalence (JZQ(45),NAB)
      equivalence (JZQ(57),KWA)
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
      equivalence (RZQ( 28),XKDST)
      equivalence (KZQ( 27),KURIN)
      equivalence (KZQ( 55),NHTSW)
      equivalence (KZQ( 44),NCOSW)
      equivalence (KZQ(184),NLY  )
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
C     !EJECT
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ(307),IQALO)
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
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- APULIA      as of 1994 Nov 02
      real*8      RAYSLM
      common      /APULIA/ RAYSLM
C     Wavelength crossover for Rayleigh scattering computations.
C     .
C     !DASH
C     !EJECT
      external  FRANK, MESHED, ABORT, HI, BYE
      intrinsic min, max
C
      dimension VAL(2)
C
      data VAL /'off', 'on '/
C
      call HI ('HEDRE')
C     !BEG
      if(XKDST.le.ZERO) then
C----   Dust opacity
        KOPAC(14) = 0
        KOPAC(15) = 0
      end if
      if((KURIN.le.0).or.(KURIN.gt.10)) then
C----   Statistical lines opacity
        KOPAC(22) = 0
        KOPAC(23) = 0
      end if
      if((NCP.le.0).or.(NAB.le.0)) then
C----   Composite lines opacity
        KOPAC(24) = 0
        KOPAC(25) = 0
      end if
      if((KWA.le.0).or.(IQALO.le.0)) then
C----   "Averaged" lines opacity
        KOPAC(31) = 0
        KOPAC(32) = 0
      end if
      if(NHTSW.le.0) then
C----   H2 Rayleigh scattering
        KOPAC(28) = 0
      end if
      if((LCOW.le.0).or.(NCOSW.le.0)) then
C----   CO-lines absorption and scattering
        KOPAC(27) = 0
        KOPAC(30) = 0
      end if
C     !EJECT
C---- Higher H Ly lines
      if(NLY.lt.3) then
        KOPAC(34) = 0
        KOPAC(36) = 0
        KOPAC(37) = 0
      end if
C---- Helium
      call FRANK ('HE ', 0, AB, dummy, dummy, dummy, LODE)
      if((LODE.eq.0).or.(AB.eq.ZERO)) then
        KOPAC(20) = 0
        KOPAC(21) = 0
        KOPAC(29) = 0
        KOPAC(33) = 0
        KOPAC(35) = 0
      end if
C---- Sulfur
      call FRANK ('S  ', 0, AB, dummy, dummy, dummy, LODE)
      if((LODE.eq.0).or.(AB.eq.ZERO)) then
        KOPAC( 6) = 0
      end if
C---- Silicon
      call FRANK ('SI ', 0, AB, dummy, dummy, dummy, LODE)
      if((LODE.eq.0).or.(AB.eq.ZERO)) then
        KOPAC( 7) = 0
      end if
C---- Magnesium
      call FRANK ('MG ', 0, AB, dummy, dummy, dummy, LODE)
      if((LODE.eq.0).or.(AB.eq.ZERO)) then
        KOPAC( 8) = 0
      end if
C---- Carbon
      call FRANK ('C  ', 0, AB, dummy, dummy, dummy, LODE)
      if((LODE.eq.0).or.(AB.eq.ZERO)) then
        KOPAC(10) = 0
        KOPAC(27) = 0
        KOPAC(30) = 0
        KOPAC(41) = 0
      end if
C---- Aluminum
      call FRANK ('AL ', 0, AB, dummy, dummy, dummy, LODE)
      if((LODE.eq.0).or.(AB.eq.ZERO)) then
        KOPAC(12) = 0
      end if
C---- Iron
      call FRANK ('FE ', 0, AB, dummy, dummy, dummy, LODE)
      if((LODE.eq.0).or.(AB.eq.ZERO)) then
        KOPAC(17) = 0
      end if
C     !EJECT
C---- Sodium
      call FRANK    ('NA ', 0, AB, dummy, dummy, dummy, LODE)
      if((LODE.eq.0).or.(AB.eq.ZERO)) then
        KOPAC(18) = 0
      end if
C---- Calcium
      call FRANK    ('CA ', 0, AB, dummy, dummy, dummy, LODE)
      if((LODE.eq.0).or.(AB.eq.ZERO)) then
        KOPAC(19) = 0
      end if
C---- Oxygen
      call FRANK    ('O  ', 0, AB, dummy, dummy, dummy, LODE)
      if((LODE.eq.0).or.(AB.eq.ZERO)) then
        KOPAC(27) = 0
        KOPAC(30) = 0
        KOPAC(38) = 0
        KOPAC(39) = 0
        KOPAC(40) = 0
        KOPAC(42) = 0
        KOPAC(43) = 0
        KOPAC(44) = 0
        KOPAC(45) = 0
      end if
C
C---- Check for discrepancies
      if((KOPAC(4)-KOPAC(16)).ne.0) then
        K04 = max(min(KOPAC( 4),1),0)+1
        K16 = max(min(KOPAC(16),1),0)+1
        call MESHED ('HEDRE', 1)
        write (LUEO,100) VAL(K04),VAL(K16),RAYSLM
  100   format(' ','It appears that for this run, kap(4) = ',A3,
     $             ' and kap(16) = ',A3,', which is inconsistent.'//
     $         ' ','Note that since a component of Lyman Sct. is a ',
     $             'continuation of H Rayleigh Sct. for'/
     $         ' ','wavelengths <',1PE12.4,', kap(4) and kap(16) ',
     $             'should either both be on, or both be off.')
        call ABORT
      end if
C     !END
      call BYE ('HEDRE')
C
      return
      end
