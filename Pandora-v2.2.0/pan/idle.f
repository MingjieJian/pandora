      subroutine IDLE
     $(LU)
C
C     Rudolf Loeser, 2003 Jun 20
C---- Prints an explanation, for EBISSA.
C     (This is version 3 of IDLE.)
C     !DASH
      save
C     !DASH
      real*8 WBD, WBDIR, WPOP
      integer IQAMB, IQVLP, KAMB, KVLG, LU
      logical AMB, DIR, KILROY, VEL
      character MESS*90
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
      equivalence (RZQ( 25),WPOP )
      equivalence (RZQ( 45),WBD  )
      equivalence (RZQ(109),WBDIR)
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
      equivalence (IQQ(264),IQAMB)
      equivalence (IQQ(239),IQVLP)
C     !EJECT
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(32),KAMB )
      equivalence (LEST(47),KVLG )
C     !DASH
      external LINER, HI, BYE
C
      data KILROY /.true./
C
      call HI ('IDLE')
C     !BEG
      if(LU.gt.0) then
        if(KILROY) then
          KILROY = .false.
          write (MESS,100)
  100     format('(Weight = 1.0 gives full weight to latest results ',
     $           'and none to previous ones.)')
        end if
C
        AMB = (KAMB.gt.0).and.(IQAMB.le.0)
        VEL = (KVLG.gt.0).and.(IQVLP.le.0)
        DIR = AMB.or.VEL
C
        call LINER     (1,LU)
        write (LU,101) WPOP,MESS,WBD,MESS
  101   format(' ',  'WPOP ',1PE12.2,' Weight for N. ',A/
     $         ' ',  'WBD  ',  E12.2,' Weight for B. ',A)
        if(DIR) then
          write (LU,102) WBDIR
  102     format(' ','WBDIR',1PE12.2,' Weight for "B-direct".')
        end if
      end if
C     !END
      call BYE ('IDLE')
C
      return
      end
