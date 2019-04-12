      subroutine LUSH
C
C     Rudolf Loeser, 2002 Jul 12
C---- Initializes the printout index file.
C     !DASH
      save
C     !DASH
      integer IQMIX, LUIX
C     !COM
C---- HEADER      as of 1984 Apr 23
      character   HEAD*80
      common      /HEADER/ HEAD
C     Copy of the "HEADER" line for this run.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS(11),LUIX )
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
      equivalence (IQQ(217),IQMIX)
C     !DASH
      external LINER, HI, BYE
C
      call HI ('LUSH')
C     !BEG
      if(IQMIX.gt.0) then
        write (LUIX,100) HEAD
  100   format(' ',A)
        call LINER (2,LUIX)
      end if
C     !END
      call BYE ('LUSH')
C
      return
      end
