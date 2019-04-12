      subroutine BOTTLE
     $(NO,LU)
C
C     Rudolf Loeser, 2006 Feb 28
C---- Sets LUN for full Line Source Function printout, for PSHAW.
C     !DASH
      save
C     !DASH
      integer IQPFU, LU, NO
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
      equivalence (IQQ(342),IQPFU)
C     !DASH
      external ZEUS, LINER, HI, BYE
C
      call HI ('BOTTLE')
C     !BEG
      call ZEUS      (NO, IQPFU, LU)
C
      if(LU.gt.0) then
        call LINER   (3, LU)
        write (LU,100)
  100   format(' ','>>>>>>>>>> To omit the following supplementary ',
     $             'printouts, set option LSFFULL = off.')
      else
        if(NO.gt.0) then
          call LINER (3, NO)
          write (NO,101)
  101     format(' ','>>>>>>>>>> To see available supplementary ',
     $               'printouts, set option LSFFULL = on.')
        end if
      end if
C     !END
      call BYE ('BOTTLE')
C
      return
      end
