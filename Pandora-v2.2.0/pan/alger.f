      subroutine ALGER
     $(NO)
C
C     Rudolf Loeser, 1992 May 26
C---- Prints a message for ATOM.
C     !DASH
      save
C     !DASH
      integer IQDAS, NO
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
      equivalence (IQQ(293),IQDAS)
C     !DASH
      external LINER, HI, BYE
C
      call HI ('ALGER')
C     !BEG
      if((NO.gt.0).and.(IQDAS.le.0)) then
        write (NO,100)
  100   format(' ',8X,'* To save the computed default parameter ',
     $             'values in file .msc, turn option ATOMSAV on.')
        call LINER (1, NO)
      end if
C     !END
      call BYE ('ALGER')
C
      return
      end
