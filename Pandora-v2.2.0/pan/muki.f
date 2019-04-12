      subroutine MUKI
     $(DUMP)
C
C     Rudolf Loeser, 2004 Mar 17
C----
C     !DASH
      save
C     !DASH
      integer IQROD, IQROP, MO
      logical DUMP
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
      equivalence (IQQ(199),IQROP)
      equivalence (IQQ(120),IQROD)
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
      call HI ('MUKI')
C     !BEG
      DUMP = (MO.gt.0).and.(IQROP.gt.0).and.(IQROD.gt.0)
C     !END
      call BYE ('MUKI')
C
      return
      end
