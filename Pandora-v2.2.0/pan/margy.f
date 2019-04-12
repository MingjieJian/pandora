      subroutine MARGY
     $(X)
C
C     Rudolf Loeser, 2002 Sep 09
C---- Prints input populations and departure coefficients of the
C     ion-of-the-run.
C     !DASH
      save
C     !DASH
      real*8 X
      integer IQINB, NO
C     !COM
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
      equivalence (IQQ(324),IQINB)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
C     !EJECT
      external PRIAM, LINER, GARYM, HI, BYE
C
      dimension X(*)
C
      call HI ('MARGY')
C     !BEG
      call PRIAM   (NO, 'ND, NK, + BD', 12)
      call LINER   (1, NO)
      write (NO,100)
  100 format(' ','Output controlled by option INNBPRNT.')
      if(IQINB.gt.0) then
        call GARYM (X, 'Input')
      end if
C     !END
      call BYE ('MARGY')
C
      return
      end
