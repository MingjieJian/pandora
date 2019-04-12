      subroutine HEALALL
     $(X,IX,W,IW,XCBL)
C
C     Rudolf Loeser, 1995 Apr 07
C---- Sets up data for incident coronal radiation continuum calculation.
C     (This is version 2 of HEALALL.)
C     !DASH
      save
C     !DASH
      real*8 W, X, XCBL
      integer IQLYM, IW, IWS, IX, JJLCR, JJYCR, JN, JOPAC, JOPAT, JSTCN,
     $        MUX, NCR, NOION
      logical DOIT, SWTA, SWTB
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(32),NCR)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(116),JJLCR)
      equivalence (IZOQ(118),JJYCR)
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
      equivalence (KZQ( 35),JSTCN)
      equivalence (KZQ( 94),NOION)
C     !DASH
      external POVERTY, MORTAIN, IGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XCBL(Miklen)
      dimension XCBL(*)
C
      dimension JN(2)
      equivalence
     $(JN( 1),JOPAC ),(JN( 2),JOPAT )
C
      call HI ('HEALALL')
C     !BEG
      SWTA = (JSTCN.le.0).and.(NOION.le.0)
      SWTB = (IQLYM.gt.0).and.(NCR.gt.0)
      DOIT = SWTA.and.SWTB
C
      if(DOIT) then
C       (Get, and allocate, IW allotment)
        call MORTAIN (JN, IWS, MUX, 'HEALALL')
C
        call POVERTY (X, XCBL, X(JJLCR), X(JJYCR), IW(JOPAC),
     $                IW(JOPAT))
C
C       (Give back IW allotment)
        call IGIVE   (IW, 'HEALALL')
      end if
C     !END
      call BYE ('HEALALL')
C
      return
      end
