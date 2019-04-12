      subroutine ILONA
     $(DUMP,DMPW)
C
C     Rudolf Loeser, 1982 Apr 02
C---- Sets up dump output units, for TAURUS.
C     !DASH
      save
C     !DASH
      integer IQLYD, IQWDD, MO
      logical DMPW, DUMP
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
      equivalence (IQQ(116),IQLYD)
      equivalence (IQQ(146),IQWDD)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external HI, BYE
C
      call HI ('ILONA')
C     !BEG
      DUMP = (MO.gt.0).and.(IQLYD.gt.0)
      DMPW = DUMP.and.(IQWDD.le.0)
C     !END
      call BYE ('ILONA')
C
      return
      end
