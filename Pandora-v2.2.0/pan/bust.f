      subroutine BUST
     $(X,W,IW)
C
C     Rudolf Loeser, 1970 Feb 11
C---- Does precalculations for Line Source Function calculations.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IQDIR, IW, MSFQM
      logical GOODF, GOODS
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(18),MSFQM)
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
      equivalence (IQQ(233),IQDIR)
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
C     !EJECT
      external AGNI, AKKAD, APSU, HALT, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
      call HI ('BUST')
C     !BEG
      if(IQDIR.gt.0) then
C
C----   Compute Line Source Function frequency integration weights
        call AGNI    (X, W, IW, GOODS, GOODF)
        if(MSFQM.gt.0) then
C----     Compute Lambda-operator Matrix for the standard TAU table
          call AKKAD (X, W, IW)
        end if
C----   Print
        call APSU    (X, MSFQM)
C
        if((.not.GOODS).or.(.not.GOODF)) then
          write (MSSLIN(1),100)
  100     format('Line Source Function standard frequency integration ',
     $           'weights calculation failed.')
          call HALT  ('BUST', 1)
        end if
C
      end if
C     !END
      call BYE ('BUST')
C
      return
      end
