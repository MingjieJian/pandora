      subroutine COMFREY
     $(X,IX,W,IW,XCBL)
C
C     Rudolf Loeser, 1995 Apr 07
C---- Sets up data for Lyman continuum calculations.
C     (This is version 2 of COMFREY.)
C     !DASH
      save
C     !DASH
      real*8 W, X, XCBL
      integer IQLYM, IW, IWS, IX, JJXCU, JJXK, JJXNU, JJYLM, JN, JOPAC,
     $        JOPAT, JSTCN, MUX, NOION
      logical DOIT
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 26),JJXNU)
      equivalence (IZOQ(260),JJXCU)
      equivalence (IZOQ( 61),JJYLM)
      equivalence (IZOQ(123),JJXK )
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
      equivalence (KZQ( 94),NOION)
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
      equivalence (IQQ( 13),IQLYM)
C     !DASH
C     !EJECT
      external PAUCITY, MORTAIN, IGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XCBL(Miklen)
      dimension XCBL(*)
C
      dimension JN(2)
      equivalence
     $(JN( 1),JOPAC),(JN( 2),JOPAT)
C
      call HI ('COMFREY')
C     !BEG
      DOIT = (JSTCN.le.0).and.(NOION.le.0).and.(IQLYM.gt.0)
C
      if(DOIT) then
C       (Get, and allocate, IW allotment)
        call MORTAIN (JN, IWS, MUX, 'COMFREY')
C
        call PAUCITY (X, XCBL, X(JJXNU), X(JJXCU), X(JJXK), X(JJYLM),
     $                IW(JOPAC), IW(JOPAT))
C
C       (Give back IW allotment)
        call IGIVE   (IW, 'COMFREY')
      end if
C     !END
      call BYE ('COMFREY')
C
      return
      end
