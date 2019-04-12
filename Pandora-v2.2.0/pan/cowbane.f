      subroutine COWBANE
     $(X,IX,W,IW,XCBL,SWAVE,SLTIT,NSH)
C
C     Rudolf Loeser, 1995 Apr 07
C---- Sets up data for Dust continuum calculations.
C     (This is version 2 of COWBANE.)
C     !DASH
      save
C     !DASH
      real*8 SLTIT, SWAVE, W, X, XCBL
      integer IQDT2, IQND2, IW, IWS, IX, JJLDT, JJYDT, JN, JOPAC, JOPAT,
     $        JSTCN, MUX, NDT, NSH
      logical DOIT, SWTA, SWTB
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(21),NDT)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 90),JJLDT)
      equivalence (IZOQ( 89),JJYDT)
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
      equivalence (IQQ(212),IQND2)
      equivalence (IQQ( 99),IQDT2)
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
C     !DASH
      external MAIDU, MORTAIN, IGIVE, HI, BYE
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
      call HI ('COWBANE')
C     !BEG
      SWTA = (IQDT2.gt.0).and.(IQND2.gt.0)
      SWTB = (NDT.gt.0).and.(JSTCN.le.0)
      DOIT = SWTA.and.SWTB
C
      if(DOIT) then
C       (Get, and allocate, IW allotment)
        call MORTAIN (JN, IWS, MUX, 'COWBANE')
C
        call MAIDU   (X, XCBL, X(JJLDT), X(JJYDT), IW(JOPAC),
     $                IW(JOPAT), SWAVE, SLTIT, NSH)
C
C       (Give back IW allotment)
        call IGIVE   (IW, 'COWBANE')
      end if
C     !END
      call BYE ('COWBANE')
C
      return
      end
