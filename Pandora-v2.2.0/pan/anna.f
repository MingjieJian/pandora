      subroutine ANNA
     $(NO,EMU,EMUF,WMUF,FRR,ISSV,VXN,HND,HNDF,FNH)
C
C     Rudolf Loeser, 1980 Jun 27
C---- Prints input for SPECTRUM Calculations.
C     (This is version 5 of ANNA.)
C     !DASH
      save
C     !DASH
      real*8 EMU, EMUF, FNH, FRR, HND, HNDF, VXN, WMUF
      integer IQCFX, IQEMI, IQLGT, IQPH2, ISSV, NO
      logical DOIT, FLAG
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
      equivalence (IQQ(  5),IQPH2)
      equivalence (IQQ(  9),IQLGT)
      equivalence (IQQ( 55),IQEMI)
      equivalence (IQQ( 80),IQCFX)
C     !DASH
C     !EJECT
      external PLOY, HI, BYE
C
C               ISSV(NVX), HNDF(NFH), FRR(MRR), VXN(N,NVX), WMUF(LF),
      dimension ISSV(*),   HNDF(*),   FRR(*),   VXN(*),     WMUF(*),
C
C               EMU(L), EMUF(LF), HND(N), FNH(NFH)
     $          EMU(*), EMUF(*),  HND(*), FNH(*)
C
      call HI ('ANNA')
C     !BEG
      FLAG = (IQLGT.gt.0).or.(IQEMI.gt.0).or.(IQCFX.gt.0)
      DOIT = (IQPH2.gt.0).and.FLAG
C
      if(DOIT) then
        call PLOY (NO, EMU, EMUF, WMUF, FRR, ISSV, VXN, HND, HNDF, FNH)
      end if
C     !END
      call BYE ('ANNA')
C
      return
      end
