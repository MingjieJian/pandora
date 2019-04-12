      subroutine YINNOP
     $(KB1WS,KB1WA,KB1WB,WBDIR,PRNT)
C
C     Rudolf Loeser, 2003 Jun 19
C---- Sets up print switches and prints header, for MULIAK.
C     (This is version 2 of YINNOP.)
C     !DASH
      save
C     !DASH
      real*8 WBDIR
      integer IQAMB, IQVLP, KB1WA, KB1WB, KB1WS, MO
      logical PRNT
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
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
      equivalence (IQQ(264),IQAMB)
      equivalence (IQQ(239),IQVLP)
C     !DASH
      external ABJECT, LINER, HI, BYE
C     !EJECT
C
      call HI ('YINNOP')
C     !BEG
      PRNT = (MO.gt.0).and.((IQAMB.gt.0).or.(IQVLP.gt.0))
C
      if(PRNT) then
        call ABJECT (MO)
C
        write (MO,100)
  100   format(' ','Comparison of B-normal, B-direct, and the final ',
     $             '(combined) result; (output depends on options ',
     $             'AMBPRNT or VELGPRNT).'//
     $         ' ','(Note: only the results for  Level 1  are shown.)')
        call LINER  (3, MO)
        write (MO,101) KB1WS,KB1WA,KB1WB,WBDIR
  101   format(' ','Weight selection switch KB1WS =',I5//
     $         ' ','                                   1 means: ',
     $             'WT = 0'/
     $         ' ','                                   2 means: ',
     $             'WT = 1 (default)'/
     $         ' ','                                   3 means: ',
     $             'WT = ZION'/
     $         ' ','                                   4 means: ',
     $             'WT = 1 to 0, using KB1WA =',I5,
     $             ' and KB1WB =',I5//
     $         ' ','     Overall multiplier WBDIR =',1PE12.2)
        call LINER  (3, MO)
        write (MO,102)
  102   format(' ',9X,'BD1(normal)',5X,'BD1(direct)',8X,'Compared',
     $             3X,'WT (= weight)',6X,'BD1(final)')
        call LINER  (1, MO)
      end if
C     !END
      call BYE ('YINNOP')
C
      return
      end
