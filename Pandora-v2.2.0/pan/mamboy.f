      subroutine MAMBOY
     $(INDX,LU,DUMP)
C
C     Rudolf Loeser, 2003 Oct 28
C---- Sets up output controls for BOMBAY.
C     !DASH
      save
C     !DASH
      integer INDX, IQSSD, IQSSP, LU, LUEO, MO
      logical DUMP
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
      equivalence (IQQ(148),IQSSP)
      equivalence (IQQ(269),IQSSD)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
      equivalence (LUNITS( 6),LUEO )
C     !DASH
C     !EJECT
      external HI, BYE
C
      call HI ('MAMBOY')
C     !BEG
      LU   = 0
      DUMP = .false.
      if(IQSSP.gt.0) then
        LU = MO
      end if
      if((INDX.gt.0).or.(IQSSD.gt.0)) then
        LU   = LUEO
        DUMP = .true.
      end if
C     !END
      call BYE ('MAMBOY')
C
      return
      end
