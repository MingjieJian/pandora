      subroutine REDLEG
     $(X,IX,W,IW,XCBL,SWAVE,SLTIT,NSH)
C
C     Rudolf Loeser, 1995 Apr 07
C---- Sets up data for H- continuum calculations.
C     (This is version 2 of REDLEG.)
C     !DASH
      save
C     !DASH
      real*8 SLTIT, SWAVE, W, X, XCBL
      integer IQHMS, IW, IWS, IX, JJLHM, JJYHM, JN, JOPAC, JOPAT, JSTCN,
     $        MHM, MUX, NSH
      logical DOIT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(22),MHM)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 84),JJLHM)
      equivalence (IZOQ( 87),JJYHM)
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
      equivalence (IQQ( 68),IQHMS)
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
      external ROMEO, MORTAIN, IGIVE, HI, BYE
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
      call HI ('REDLEG')
C     !BEG
      DOIT = (IQHMS.gt.0).and.(MHM.gt.0).and.(JSTCN.le.0)
C
      if(DOIT) then
C       (Get, and allocate, IW allotment)
        call MORTAIN (JN,IWS,MUX,'REDLEG')
C
        call ROMEO   (X,XCBL,X(JJLHM),X(JJYHM),IW(JOPAC),IW(JOPAT),
     $                SWAVE,SLTIT,NSH)
C
C       (Give back IW allotment)
        call IGIVE   (IW,'REDLEG')
      end if
C     !END
      call BYE ('REDLEG')
C
      return
      end
