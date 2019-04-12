      subroutine TWOBAM
     $(LUD,LUA)
C
C     Rudolf Loeser, 2004 May 21
C---- Sets up output units, and prints heading, for FLUE.
C     (This is version 2 of TWOBAM.)
C     !DASH
      save
C     !DASH
      integer IQANA, IQDPW, LUA, LUD, MLSFP, MO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
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
      equivalence (LEST(43),MLSFP)
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
      equivalence (IQQ( 25),IQDPW)
      equivalence (IQQ(  1),IQANA)
C     !DASH
C     !EJECT
      external ZEUS, PRIAM, HI, BYE
C
      call HI ('TWOBAM')
C     !BEG
C---- Set up output units
      call ZEUS  (MO, IQDPW, LUD)
      call ZEUS  (MO, IQANA, LUA)
C
      if(MLSFP.le.0) then
        LUD = 0
        LUA = 0
      end if
C
C---- Print heading
      call PRIAM (LUD, 'DAMPING', 7)
C     !END
      call BYE ('TWOBAM')
C
      return
      end
