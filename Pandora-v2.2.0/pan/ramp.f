      subroutine RAMP
     $(X,NO)
C
C     Rudolf Loeser, 2006 May 24
C---- Supervises the A-TROUBLES summary.
C     (This is version 2 of RAMP.)
C     !DASH
      save
C     !DASH
      real*8 X, YAFUL, YALYM, YASYM
      integer IQLYM, JJYAF, KNEGA, NO
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(264),JJYAF)
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
      equivalence (IQQ( 13),IQLYM)
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
      equivalence (REST( 6),YASYM)
      equivalence (REST( 7),YAFUL)
      equivalence (REST( 3),YALYM)
      equivalence (LEST(56),KNEGA)
C     !DASH
C     !EJECT
      external PRIAM, LINER, VAMP, DAMP, HI, BYE
C
      dimension X(*)
C
      call HI ('RAMP')
C     !BEG
      if((NO.gt.0).and.(KNEGA.gt.0)) then
        call PRIAM  (NO, 'A-TROUBLES', 10)
        call LINER  (1, NO)
        write (NO,100)
  100   format(' ','This output appears because problem(s) occurred ',
     $             'while calculating frequency integration weights.')
        call LINER  (1,NO)
        call VAMP   (YASYM, 'Half-profile A', NO)
        call VAMP   (YAFUL, 'Full-profile A', NO)
        if(IQLYM.gt.0) then
          call VAMP (YALYM, 'Lyman A', NO)
        end if
        call DAMP   (X(JJYAF), NO)
      end if
C     !END
      call BYE ('RAMP')
C
      return
      end
