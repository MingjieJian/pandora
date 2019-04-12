      subroutine ZOLTAN
C
C     Rudolf Loeser, 1995 Apr 14
C---- Driver for ADONIS.
C     (This is version 2 of ZOLTAN.)
C     !DASH
      save
C     !DASH
      integer IQIXD, LUEO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
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
      equivalence (IQQ(174),IQIXD)
C     !DASH
      external MESHED, LINER, ADONIS, MASHED, HI, BYE
C
      call HI ('ZOLTAN')
C     !BEG
      if(IQIXD.gt.0) then
        call MESHED ('ZOLTAN', 2)
C
        call ADONIS
C
        call LINER  (2, LUEO)
        write (LUEO,100)
  100   format(' ',111X,'(Option INDXDMP)')
        call MASHED ('ZOLTAN')
      end if
C     !END
      call BYE ('ZOLTAN')
C
      return
      end
