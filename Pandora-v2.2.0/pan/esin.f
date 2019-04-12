      subroutine ESIN
     $(LU)
C
C     Rudolf Loeser, 2003 Aug 21
C---- Prints a switch status summary for ESNI.
C     (This is version 3 of ESIN.)
C     !DASH
      save
C     !DASH
      integer IQBNV, IQNPL, IQNUM, IQSNB, LU, jummy
      character BNV*3, NPL*3, NUM*3, SNB*3
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
      equivalence (IQQ( 29),IQNUM)
      equivalence (IQQ(118),IQSNB)
      equivalence (IQQ(325),IQBNV)
      equivalence (IQQ( 34),IQNPL)
C     !DASH
      external ONOFF, HI, BYE
C
      call HI ('ESIN')
C     !BEG
      if(LU.gt.0) then
        call ONOFF (IQNUM, jummy, NUM)
        call ONOFF (IQSNB, jummy, SNB)
        call ONOFF (IQBNV, jummy, BNV)
        call ONOFF (IQNPL, jummy, NPL)
C
        write (LU,100) NUM,SNB,BNV,NPL
  100   format(' ','In this run: option POPPRNT is ',A3,', NBPRNT is ',
     $             A3,', PDETPRNT is ',A3,', and POPGRAF is ',A3,'.')
      end if
C     !END
      call BYE ('ESIN')
C
      return
      end
