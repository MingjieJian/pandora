      subroutine KYOTO
     $(LINK,KTRU,DOIT)
C
C     Rudolf Loeser, 2004 Sep 07
C---- Sets control switches for BERMUDA.
C     (This is version 2 of KYOTO.)
C     !DASH
      save
C     !DASH
      integer IQTRC, KTRU, LINK
      logical DOIT
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
      equivalence (IQQ(299),IQTRC)
C     !DASH
      external HI, BYE
C
      call HI ('KYOTO')
C     !BEG
      DOIT = .true.
C
      if(LINK.eq.3) then
        if(IQTRC.gt.0) then
          KTRU = 1
        else
          KTRU = 0
          DOIT = .false.
        end if
      else
        KTRU = 0
      end if
C     !END
      call BYE ('KYOTO')
C
      return
      end
