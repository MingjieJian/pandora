      subroutine TRAVIS
     $(X,NTMX,TAB,NT)
C
C     Rudolf Loeser, 2003 Mar 05
C---- Sets up standard rates integration wavelengths table.
C
C     NOTE: The user must allow NTMX (input) large enough for TAB to
C     hold all the data. Upon return, NT will be the actual number of
C     data values in TAB.
C
C     (This is version 2 of TRAVIS.)
C     !DASH
      save
C     !DASH
      real*8 BAND, HUNDRD, TAB, WRTMN, WRTMX, X
      integer NT, NTMX
C     !COM
C---- ISOLA       as of 1997 Nov 19
      real*8      WAVEDEL
      common      /ISOLA/ WAVEDEL
C     Two Continuum Wavelength values are "equal" if their
C     relative difference is less than WAVEDEL.
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
      equivalence (RZQ(166),WRTMN)
      equivalence (RZQ(167),WRTMX)
C     !DASH
C     !EJECT
      external TRIVIAL, EUPHEM, TRUNK, TRISH, CRUSH, CLASH, SMASH,
     $         HI, BYE
C
      dimension X(*)
C
C               TAB(NTMX)
      dimension TAB(*)
C
      data HUNDRD /1.D2/
C
      call HI ('TRAVIS')
C     !BEG
      BAND = WAVEDEL*HUNDRD
C
      NT = 0
C
C---- Bound-free absorption edges
      call TRIVIAL (NTMX, TAB, NT, WRTMN, WRTMX, BAND)
C
C---- H Ly background lines
      call EUPHEM  (X, NTMX, TAB, NT, WRTMN, WRTMX)
C---- He-I background lines
      call TRUNK   (X, NTMX, TAB, NT, WRTMN, WRTMX)
C---- He-II background lines
      call TRISH   (X, NTMX, TAB, NT, WRTMN, WRTMX)
C---- O-I background
      call CRUSH   (X, NTMX, TAB, NT, WRTMN, WRTMX)
C---- O-II background
      call CLASH   (X, NTMX, TAB, NT, WRTMN, WRTMX)
C---- O-III background
      call SMASH   (X, NTMX, TAB, NT, WRTMN, WRTMX)
C     !END
      call BYE ('TRAVIS')
C
      return
      end
