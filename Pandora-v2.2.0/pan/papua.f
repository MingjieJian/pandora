      subroutine PAPUA
     $(BNAME,LABEL)
C
C     Rudolf Loeser, 1981 Dec 04
C---- Makes a ray description.
C     !DASH
      save
C     !DASH
      real*8 BNAME
      integer IQSFS, IRAY, IRY, KB, LLXI, NSHL
      character BLANK*1, HALF*13, LABEL*40
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(38),KB )
C
C---- ARCHER      as of 2004 May 12
      integer     NNKOD, NNKODS
      parameter   (NNKOD=3)
C     (Be sure to recompile POD when changing NNKOD.)
      dimension   NNKODS(NNKOD)
      common      /ARCHER/ NNKODS
C     Diana/Orion Data Blocks control parameters.
C
C     (These parameters are packed and unpacked by "POD".)
C       LLXI - frequency index.
C       IRAY - angle or ray index (Orion only: expanding atmosphere);
C              (in the spherical case, Shell rays are enumerated first,
C              followed by Disk rays).
C       NRAY - number of depth levels intersected by this ray;
C              (differs from N only for Shell rays).
      equivalence (NNKODS( 1),LLXI  )
      equivalence (NNKODS( 2),IRAY  )
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
      equivalence (LEST( 4),NSHL )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !EJECT
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
C     !DASH
      external POD, HI, BYE
C
      call HI ('PAPUA')
C     !BEG
      call POD (2, BNAME)
C
      if(LLXI.lt.KB) then
        HALF = '(blue half), '
      else if(LLXI.gt.KB) then
        HALF = '(red half),  '
      else
        HALF = '(center),    '
      end if
C
      write (LABEL(1:26),100) LLXI,HALF
  100 format('Frequency #',I2,A13)
C
      if(IQSFS.le.0) then
        LABEL(27:40) = BLANK
      else
        if(IRAY.le.NSHL) then
          IRY = IRAY
          write (LABEL(27:40),101) IRY
  101     format('Shell ray #',I2)
        else
          IRY = IRAY-NSHL
          write (LABEL(27:40),102) IRY
  102     format('Disk ray #',I2)
        end if
      end if
C     !END
      call BYE ('PAPUA')
C
      return
      end
