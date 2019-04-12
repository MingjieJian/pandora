      subroutine TALE
     $(N,NL,RKI)
C
C     Rudolf Loeser, 1974 Dec 26
C---- Writes out RK1 for iterative analysis.
C     !DASH
      save
C     !DASH
      real*8 RKI
      integer IQIRK, KOLEV, KRK1, N, NL
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
      equivalence (KZQ( 33),KOLEV)
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
      equivalence (IQQ(112),IQIRK)
C     !DASH
C     !EJECT
      external BERWYN, HI, BYE
C
C               RKI(N,NL)
      dimension RKI(N,*)
C
      data KRK1 /4/
C
      call HI ('TALE')
C     !BEG
      if(IQIRK.gt.0) then
        call BERWYN (KRK1,'Tale','Rk-KOLEV',KOLEV,0,RKI(1,KOLEV),N,
     $               .true.)
      end if
C     !END
      call BYE ('TALE')
C
      return
      end
