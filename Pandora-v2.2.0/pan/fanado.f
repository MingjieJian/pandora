      subroutine FANADO
     $(LU)
C
C     Rudolf Loeser, 2003 Aug 21
C---- Prints a switch status summary for DAFANO.
C     !DASH
      save
C     !DASH
      integer IQAMB, IQOST, IQRGR, IQRGS, IQVLP, IRATE, LU, jummy
      character AMB*3, OST*3, RGR*3, RGS*3, VLP*3
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ(121),IRATE)
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
      equivalence (IQQ( 24),IQOST)
      equivalence (IQQ( 95),IQRGR)
      equivalence (IQQ(264),IQAMB)
      equivalence (IQQ(239),IQVLP)
      equivalence (IQQ(309),IQRGS)
C     !DASH
C     !EJECT
      external ONOFF, HI, BYE
C
      call HI ('FANADO')
C     !BEG
      if(LU.gt.0) then
        call ONOFF (IQOST, jummy, OST)
        call ONOFF (IQRGR, jummy, RGR)
        call ONOFF (IQAMB, jummy, AMB)
        call ONOFF (IQVLP, jummy, VLP)
        call ONOFF (IQRGS, jummy, RGS)
C
        write (LU,100) OST,RGR,AMB,VLP,RGS,IRATE
  100   format(' ','In this run: option RATEPRNT is ',A3,', RATEGRAF',
     $             ' is ',A3,', AMBPRNT is ',A3,', VLGPRNT is ',A3,
     $             ', and RATESUMM is ',A3,';'/
     $         ' ','the index IRATE =',I5,'.'//
     $         ' ','Printout also depends on option ALL, and on ',
     $             'whether any rates need to be recalculated in ',
     $             'later overall iterations.')
      end if
C     !END
      call BYE ('FANADO')
C
      return
      end
