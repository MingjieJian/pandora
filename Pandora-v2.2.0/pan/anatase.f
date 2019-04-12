      subroutine ANATASE
     $(X)
C
C     Rudolf Loeser, 1984 Aug 14
C---- Computes MHL, for ECHO.
C     (This is version 2 of ANATASE.)
C     !DASH
      save
C     !DASH
      real*8 X
      integer IQHMO, IQHMS, JJLHM, JSTCN, LUEO, MHL, MHM
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
      equivalence (LEST( 5),MHL  )
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !EJECT
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
      equivalence (IQQ(183),IQHMO)
      equivalence (IQQ( 68),IQHMS)
C     !DASH
      external ALMAND, MESHED, MASHED, HI, BYE
C
      dimension X(*)
C
      call HI ('ANATASE')
C     !BEG
      DOIT = (JSTCN.eq.0).and.(IQHMS.gt.0)
C
      if(DOIT) then
        if(IQHMO.gt.0) then
          MHL = MHM
        else
          call ALMAND (MHL, X(JJLHM), MHM)
        end if
        MHL = MHL+1
C
        if(MHL.lt.2) then
          call MESHED ('ANATASE', 3)
          write (LUEO,100) MHL
  100     format(' ','Trouble in ANATASE: MHL =',I4,' - not enough ',
     $               'wavelengths for H- calculation.')
          call MASHED ('ANATASE')
        end if
      end if
C     !END
      call BYE ('ANATASE')
C
      return
      end
