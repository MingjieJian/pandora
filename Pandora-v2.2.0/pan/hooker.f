      subroutine HOOKER
     $(I,NW,DL,PRINT)
C
C     Rudolf Loeser, 2005 Feb 28
C---- Determines whether to print continuum results.
C
C---- Assumes that ASTARTE has assured NW = K.
C     !DASH
      save
C     !DASH
      real*8 DL
      integer I, IPRDF, IQPRP, NW
      logical PRINT
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
      equivalence (KZQ( 99),IPRDF)
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
      equivalence (IQQ( 73),IQPRP)
C     !DASH
C     !EJECT
      external BROME, HI, BYE
C
C               DL(KM)
      dimension DL(*)
C
      call HI ('HOOKER')
C     !BEG
      PRINT = .false.
C
      if(IQPRP.gt.0) then
        call BROME (IPRDF, DL, NW, I, PRINT)
      end if
C     !END
      call BYE ('HOOKER')
C
      return
      end
