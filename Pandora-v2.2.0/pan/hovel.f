      subroutine HOVEL
     $(X,IX,W,IW,XCBL)
C
C     Rudolf Loeser, 2005 Jul 20
C---- Sets up data for standard rates integrations wavelengths
C     continuum calculations.
C     !DASH
      save
C     !DASH
      real*8 STAB, W, X, XCBL, YRATS
      integer IPNT, IQUTR, IQUWT, IW, IWS, IX, JN, JOPAC, JOPAT, JSTCN,
     $        MUX, NOION, NT
      logical DOIT, SWTA, SWTB
C     !COM
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
      equivalence (KZQ( 35),JSTCN)
      equivalence (RZQ(168),YRATS)
C
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
      equivalence (IQQ( 53),IQUTR)
      equivalence (IQQ( 77),IQUWT)
C     !EJECT
C---- NARITO      as of 2005 Jul 22
      integer     NTMXSW
C     (Remember to recompile users when changing NTMXSW.)
      parameter   (NTMXSW=10000)
C     Upper limit for standard rates integration wavelengths.
C     .
C     !DASH
      external MORTAIN, IGIVE, BERSERK, TROLL, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XCBL(Miklen)
      dimension XCBL(*)
C
      dimension STAB(NTMXSW), IPNT(NTMXSW)
C
      dimension JN(2)
      equivalence
     $(JN( 1),JOPAC),(JN( 2),JOPAT)
C
      call HI ('HOVEL')
C     !BEG
      SWTA = (NOION.le.0).and.(JSTCN.le.0)
      SWTB = (IQUTR.le.0).and.(IQUWT.gt.0)
      DOIT = SWTA.and.SWTB
C
      if(DOIT) then
C       (Get, and allocate, IW allotment)
        call MORTAIN (JN, IWS, MUX, 'HOVEL')
C
        call BERSERK (X, W, IW, NTMXSW, STAB, IPNT, NT)
        call TROLL   (X, XCBL, STAB, NT, YRATS, IW(JOPAC), IW(JOPAT))
C
C       (Give back IW allotment)
        call IGIVE   (IW, 'HOVEL')
      end if
C     !END
      call BYE ('HOVEL')
C
      return
      end
