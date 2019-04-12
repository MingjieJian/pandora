      subroutine TRENT
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1980 May 23
C---- Input reading and printing, and files initializations.
C     (This is version 2 of TRENT.)
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IQDLA, IW, IX, LUEO, LUIN
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 1),LUIN )
      equivalence (LUNITS( 6),LUEO )
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
      equivalence (IQQ(316),IQDLA)
C     !DASH
C     !EJECT
      external TAY, LACK, TOP, RESABRT, PEEL, COQUET, JIM, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      call HI ('TRENT')
C     !BEG
C---- Read LAST parts of input
      call TAY     (X, IX, W, IW)
C
C---- Close the basic input file
      call LACK    (LUIN, LUEO)
C---- Close the auxiliary input files
      call TOP
C
C---- Print input
      call COQUET  (X, IX, W, IW)
C
      if(IQDLA.gt.0) then
C----   Delayed ABORT is on (see TANGO); turn it off now
        call RESABRT
      end if
C
C---- Initialize auxiliary input files
      call JIM
C---- Initialize auxiliary output files
      call PEEL
C     !END
      call BYE ('TRENT')
C
      return
      end
