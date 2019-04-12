      subroutine UNMIX
     $(STRING)
C
C     Rudolf Loeser, 1996 Dec 04
C---- Changes lower-case or mixed-case strings to all upper-case.
C     !DASH
      save
C     !DASH
      integer IQMXC
      character STRING*(*), TEMP*80
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
      equivalence (IQQ(317),IQMXC)
C     !DASH
      external force_upper, HI, BYE
C
      call HI ('UNMIX')
C     !BEG
      if(IQMXC.gt.0) then
        call force_upper (STRING,TEMP)
        STRING = TEMP
      end if
C     !END
      call BYE ('UNMIX')
C
      return
      end
