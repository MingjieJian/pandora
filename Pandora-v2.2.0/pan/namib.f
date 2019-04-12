      subroutine NAMIB
     $(DUMP,CALLER)
C
C     Rudolf Loeser, 2000 Jan 04
C---- Sets up dump for number density calculations.
C     !DASH
      save
C     !DASH
      integer IOMX, IOVER, IQLND
      logical DUMP, KILROY
      character CALLER*(*)
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
      equivalence (KZQ(  8),IOMX )
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
      equivalence (LEST( 2),IOVER)
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
      equivalence (IQQ(129),IQLND)
C     !DASH
C     !EJECT
      external MESHED, HI, BYE
C
      data KILROY /.true./
C
      call HI ('NAMIB')
C     !BEG
      DUMP = .false.
      if((IOVER.eq.IOMX).and.(IQLND.gt.0)) then
        if(KILROY) then
          KILROY = .false.
          DUMP   = .true.
          call MESHED (CALLER, 2)
        end if
      end if
C     !END
      call BYE ('NAMIB')
C
      return
      end
