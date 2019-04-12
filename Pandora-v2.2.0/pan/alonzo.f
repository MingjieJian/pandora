      subroutine ALONZO
     $(LU)
C
C     Rudolf Loeser, 1992 May 19
C---- Sets up printing of input PRD Jnu.
C     !DASH
      save
C     !DASH
      integer IQJIN, IQPPJ, LU, NO
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
      equivalence (IQQ(240),IQJIN)
      equivalence (IQQ(167),IQPPJ)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
C     !DASH
C     !EJECT
      external LINER, HI, BYE
C
      call HI ('ALONZO')
C     !BEG
      LU = 0
C
      if(IQJIN.gt.0) then
        LU = NO
      else
        if(IQPPJ.gt.0) then
          LU = NO
        else
          call LINER (5, NO)
          write (NO,100)
  100     format(' ','(To print input values of PRD Jnu, turn on ',
     $               'option JNUPRNT.)')
        end if
      end if
C     !END
      call BYE ('ALONZO')
C
      return
      end
