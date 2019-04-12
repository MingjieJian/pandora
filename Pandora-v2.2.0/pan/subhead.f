      subroutine SUBHEAD
     $(IOVER,IOMX,ITER,ISUB)
C
C     Rudolf Loeser, 1980 Aug 12
C---- Prints a sub-iteration heading.
C     !DASH
      save
C     !DASH
      integer IOMX, IOVER, IQEVR, ISUB, ITER, NO
      character BLANK*1, LINE*80, TIT*11
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
      equivalence (IQQ( 19),IQEVR)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
C     !EJECT
      external PRIAM, LINER, ERRHEAD, HI, BYE
C
      call HI ('SUBHEAD')
C     !BEG
      if(IQEVR.gt.0) then
C----   Section header, if needed
        write (TIT,100) ITER,IOVER
  100   format('ITER ',I3,',',I2)
        call PRIAM (NO, TIT, 11)
      end if
C
C---- Invariant printout
      LINE = BLANK
      write (LINE,101) ITER,ISUB,IOVER,IOMX
  101 format('Sub-Iteration',I3,' of',I3,', Overall-Iteration',I3,
     $       ' of',I3,'.')
C
      call LINER   (4, NO)
      write (NO,102) LINE
  102 format(' ',A)
C
C---- Write error file marker, if needed
      call ERRHEAD (LINE)
C     !END
      call BYE ('SUBHEAD')
C
      return
      end
