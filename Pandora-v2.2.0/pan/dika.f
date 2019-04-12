      subroutine DIKA
     $(GVL1,G1,DIDG1,GNVCHK,MN1,LU)
C
C     Rudolf Loeser, 1990 Jul 31
C---- Does GNVCHECK,for TARPON.
C     !DASH
      save
C     !DASH
      real*8 G1, GNVCHK, GVL1
      integer IQCGP, LU, MN1
      logical DIDG1
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
      equivalence (IQQ(251),IQCGP)
C     !DASH
      external ABJECT, LINER, TAKE, HI, BYE
C
C               GVL1(N), G1(N), GNVCHK(N)
      dimension GVL1(*), G1(*), GNVCHK(*)
C
      call HI ('DIKA')
C     !BEG
      if(DIDG1.and.(MN1.gt.0)) then
        if((IQCGP.gt.0).and.(LU.gt.0)) then
          call ABJECT (LU)
          write (LU,100)
  100     format(' ','Consistency check',92X,'(Option CHKPRNT)'//
     $           ' ','GNVCHK = Ratio = GNVL-1 / G1')
          call LINER  (2,LU)
          call TAKE   (LU,MN1,GVL1,'GNVL-1',G1,'G1',GNVCHK)
        end if
      end if
C     !END
      call BYE ('DIKA')
C
      return
      end
