      subroutine MOLTO
     $(DUMP)
C
C     Rudolf Loeser, 2003 Oct 24
C---- Sets up printout, for HSE.
C     (This is version 2 of MOLTO.)
C     !DASH
      save
C     !DASH
      integer IQHSD, MO
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
      equivalence (IQQ(132),IQHSD)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external PRIAM, HI, BYE
C
      call HI ('MOLTO')
C     !BEG
      DUMP = .false.
      if(MO.gt.0) then
        call PRIAM (MO, 'H.S.E.', 6)
        DUMP = IQHSD.gt.0
      end if
C     !END
      call BYE ('MOLTO')
C
      return
      end
