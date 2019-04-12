      subroutine TULA
     $(X)
C
C     Rudolf Loeser, 1987 Nov 18
C---- Computes NCOW, the number of wavelengths available for the
C     CO-lines cooling rates calculation.
C     (This is version 2 of TULA.)
C     !DASH
      save
C     !DASH
      real*8 X
      integer IQCOC, JJXCA, JJXCB, NCB, NCOW
      logical DOIT
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(55),NCB)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(221),JJXCA)
      equivalence (IZOQ(222),JJXCB)
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
      equivalence (IQQ(225),IQCOC)
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
      equivalence (LEST(40),NCOW )
C     !DASH
      external CANADA, HI, BYE
C
      dimension X(*)
C
      call HI ('TULA')
C     !BEG
      NCOW = 0
      DOIT = (NCB.gt.0).and.(IQCOC.gt.0)
C
      if(DOIT) then
        call CANADA (NCB, X(JJXCA), X(JJXCB), NCOW)
      end if
C     !END
      call BYE ('TULA')
C
      return
      end
