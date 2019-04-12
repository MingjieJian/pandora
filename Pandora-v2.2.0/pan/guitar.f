      subroutine GUITAR
     $(Y,DRO,GMA,XC,XP,XR,CRS,CDL,CRD,CVW,CSK,DPM,NED,ISB1,ISB2,RHW)
C
C     Rudolf Loeser, 1999 Nov 18
C---- Initializes some Line Intensity Data Block arrays.
C     (This is version 2 of GUITAR.)
C     !DASH
      save
C     !DASH
      real*8 CDL, CRD, CRS, CSK, CVW, DPM, DRO, GMA, ONE, PO5, RHW,
     $       THREE, TWO, XC, XP, XR, Y, YDEF
      integer ISB1, ISB2, LDLMX, N, NED, NOION, NT, NTLDL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 5),NT )
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
      equivalence (KZQ( 94),NOION)
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
      equivalence (LEST(33),LDLMX)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 4),THREE )
C     !DASH
C     !EJECT
      external BLIND, SET1, SETI, ONE1, HI, BYE
C
C               CDL(LDLMX,NT), CRD(LDLMX,NT), ISB2(NT), CRS(NT), Y(NT),
      dimension CDL(*),        CRD(*),        ISB2(*),  CRS(*),  Y(*),
C
C               CVW(LDLMX,NT), CSK(LDLMX,NT), NED(NT), GMA(NT), XP(NT),
     $          CVW(*),        CSK(*),        NED(*),  GMA(*),  XP(*),
C
C               ISB1(NR), XC(NT), RHW(N,NT), DRO(NT), DPM(NT), XR(NT)
     $          ISB1(*),  XC(*),  RHW(*),    DRO(*),  DPM(*),  XR(*)
C
      data PO5 /5.D-2/
C
      call HI ('GUITAR')
C     !BEG
      if(NOION.le.0) then
        NTLDL = NT*LDLMX
        call BLIND (-ONE, YDEF)
C
        call SET1  (Y  , NT,  YDEF)
        call SET1  (DRO, NT,  PO5)
        call SET1  (GMA, NT, -ONE)
        call SET1  (XC , NT,  THREE)
        call SET1  (XP , NT,  TWO)
        call SET1  (XR , NT, -ONE)
        call SET1  (CRS, NT, -ONE)
        call ONE1  (DPM, NT)
C
        call SETI  (NED , 1, NT, N)
        call SETI  (ISB1, 1, NT, 1)
        call SETI  (ISB2, 1, NT, 1)
C
        call ONE1  (CDL, NTLDL)
        call SET1  (CRD, NTLDL, -ONE)
        call SET1  (CVW, NTLDL, -ONE)
        call SET1  (CSK, NTLDL, -ONE)
C
        call ONE1  (RHW, (N*NT))
      end if
C     !END
      call BYE ('GUITAR')
C
      return
      end
