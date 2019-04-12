      subroutine EMERALD
     $(BDI)
C
C     Rudolf Loeser, 1977 Jan 19
C---- Writes out BDi (i = KOLEV) for iterative summary.
C     !DASH
      save
C     !DASH
      real*8 BDI
      integer IQIBD, KBDI, KOLEV, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (IQQ(128),IQIBD)
C     !DASH
C     !EJECT
      external BERWYN, HI, BYE
C
C               BDI(N,NL)
      dimension BDI(N,*)
C
      data KBDI /7/
C
      call HI ('EMERALD')
C     !BEG
      if(IQIBD.gt.0) then
        call BERWYN (KBDI,'Emerald','Bd-KOLEV',KOLEV,0,BDI(1,KOLEV),N,
     $               .true.)
      end if
C     !END
      call BYE ('EMERALD')
C
      return
      end
