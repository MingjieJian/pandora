      subroutine VALDRAD
     $(RKI,N,KMSS,TITLE)
C
C     Rudolf Loeser, 1977 Feb 03
C---- Supervises RKI smoothing.
C     !DASH
      save
C     !DASH
      real*8 RKI, WSM
      integer INFSM, INLSM, IQSLY, KMSS, N
      character TITLE*(*)
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
      equivalence (KZQ( 23),INFSM)
      equivalence (KZQ( 24),INLSM)
      equivalence (RZQ( 34),WSM  )
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
      equivalence (IQQ( 83),IQSLY)
C     !DASH
C     !EJECT
      external SMOG, HI, BYE
C
C               RKI(N)
      dimension RKI(*)
C
      call HI ('VALDRAD')
C     !BEG
      if(IQSLY.gt.0) then
        call SMOG (RKI,N,INFSM,INLSM,WSM,KMSS,TITLE)
      end if
C     !END
      call BYE ('VALDRAD')
C
      return
      end
