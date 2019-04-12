      subroutine ANUNIT
     $(X)
C
C     Rudolf Loeser, 1981 Apr 23
C---- Controls computation of mu-integration weights.
C     !DASH
      save
C     !DASH
      real*8 X
      integer IQEXA, IQSFS, IQSTA, JJCMU, JJWMU, JJXMU, LFLX, LG, LU,
     $        MSFGR, NO
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(34),LG )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(130),JJXMU)
      equivalence (IZOQ(131),JJCMU)
      equivalence (IZOQ(167),JJWMU)
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
      equivalence (LEST(15),LFLX )
      equivalence (LEST(21),MSFGR)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
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
      equivalence (IQQ(169),IQEXA)
      equivalence (IQQ( 31),IQSFS)
      equivalence (IQQ( 76),IQSTA)
C     !DASH
      external HAW, KACHIN, ZEUS, HI, BYE
C
      dimension X(*)
C
      call HI ('ANUNIT')
C     !BEG
      if(((MSFGR.gt.0).or.(IQEXA.gt.0)).and.(IQSFS.le.0)) then
C
        call HAW    (X(JJXMU), X(JJCMU), X(JJWMU), LFLX, LG)
C
        call ZEUS   (NO, IQSTA, LU)
        call KACHIN (LU, X(JJXMU), X(JJCMU), X(JJWMU), LFLX, LG)
C
      end if
C     !END
      call BYE ('ANUNIT')
C
      return
      end
