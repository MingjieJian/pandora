      subroutine TORGILS
     $(KPRNT,PRINT)
C
C     Rudolf Loeser, 1996 Jan 25
C---- Tells whether GIGGLE should print.
C     !DASH
      save
C     !DASH
      integer IQARD, KPRNT, LDINT
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
      equivalence (KZQ( 48),LDINT)
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
      equivalence (IQQ( 15),IQARD)
C     !DASH
      external HI, BYE
C
      call HI ('TORGILS')
C     !BEG
      PRINT = (KPRNT.gt.0).and.(IQARD.gt.0).and.(LDINT.ne.0)
C     !END
      call BYE ('TORGILS')
C
      return
      end
