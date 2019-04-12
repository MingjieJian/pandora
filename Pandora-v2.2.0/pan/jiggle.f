      subroutine JIGGLE
C
C     Rudolf Loeser, 1972 Jul 20
C---- Sets options depending on other options.
C     (This is version 2 of JIGGLE.)
C
C     The "+10" bias is removed by MUCKLE.
C     !DASH
      save
C     !DASH
      integer IQAPF, IQCPU, IQCSW, IQECL, IQEMI, IQENH, IQESD, IQFIN,
     $        IQGDS, IQHSE, IQIFF, IQINC, IQLGT, IQLID, IQORT, IQPH2,
     $        IQPPU, IQREF, IQSFO, IQSFS, IQTNO, IQUTR
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
      equivalence (IQQ(  5),IQPH2)
      equivalence (IQQ(  9),IQLGT)
      equivalence (IQQ(  6),IQECL)
      equivalence (IQQ( 55),IQEMI)
      equivalence (IQQ( 49),IQFIN)
      equivalence (IQQ(141),IQIFF)
      equivalence (IQQ( 51),IQINC)
      equivalence (IQQ( 50),IQREF)
      equivalence (IQQ( 31),IQSFS)
      equivalence (IQQ(  8),IQSFO)
      equivalence (IQQ( 60),IQGDS)
      equivalence (IQQ( 38),IQENH)
      equivalence (IQQ( 16),IQHSE)
      equivalence (IQQ( 53),IQUTR)
      equivalence (IQQ( 14),IQCSW)
      equivalence (IQQ(135),IQORT)
      equivalence (IQQ( 97),IQCPU)
      equivalence (IQQ(151),IQPPU)
      equivalence (IQQ(192),IQAPF)
      equivalence (IQQ( 40),IQESD)
      equivalence (IQQ( 43),IQTNO)
      equivalence (IQQ( 57),IQLID)
C     !DASH
C     !EJECT
      external HI, BYE
C
      call HI ('JIGGLE')
C     !BEG
      if(IQPH2.le.0) then
        IQLGT = 10
        IQECL = 10
        IQEMI = 10
      end if
C
      if(IQFIN.le.0) then
        if(IQIFF.le.0) then
          IQINC = 10
        end if
        IQREF = 10
      end if
C
      if(IQSFS.gt.0) then
        IQSFO = 11
        IQGDS = 10
        IQENH = 10
        IQHSE = 10
        if(IQTNO.gt.0) then
          IQESD = 11
          IQLID = 11
        end if
      end if
C
      if(IQUTR.le.0) then
        IQCSW = 10
      end if
C
      if(IQORT.gt.0) then
        IQGDS = 10
      end if
C
      if(IQAPF.gt.0) then
        IQCPU = 11
        IQPPU = 11
      end if
C     !END
      call BYE ('JIGGLE')
C
      return
      end
