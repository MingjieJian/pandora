      subroutine ZORA
     $(LFBV)
C
C     Rudolf Loeser, 1987 Jan 23
C---- Computes Front-face / Back-face specifier.
C     LFBV = 1 means: compute Front-face emergent radiation only;
C     LFBV = 2 means: compute Back-face emergent radiation also.
C     (This is version 2 of ZORA).
C     !DASH
      save
C     !DASH
      integer IQEBI, IQFIN, IQSFO, IQSFS, LFBV
      logical SPHERE
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
      equivalence (IQQ( 31),IQSFS)
      equivalence (IQQ(  8),IQSFO)
      equivalence (IQQ( 49),IQFIN)
      equivalence (IQQ(208),IQEBI)
C     !DASH
      external HI, BYE
C
      call HI ('ZORA')
C     !BEG
      LFBV = 1
C
      SPHERE = (IQSFS.gt.0).or.(IQSFO.gt.0)
      if((.not.SPHERE).and.(IQFIN.gt.0).and.(IQEBI.gt.0)) then
        LFBV = 2
      end if
C     !END
      call BYE ('ZORA')
C
      return
      end
