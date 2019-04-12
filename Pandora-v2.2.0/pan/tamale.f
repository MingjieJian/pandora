      subroutine TAMALE
     $(N,VX1,VXI,VXS)
C
C     Rudolf Loeser, 2003 Mar 21
C---- Sets VXS = VX1, if indicated.
C     !DASH
      save
C     !DASH
      real*8 VX1, VXI, VXS
      integer IQEXA, KVXVA, N
      logical VZERO
C     !COM
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(66),KVXVA)
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
      equivalence (IQQ(169),IQEXA)
C     !DASH
      external NAUGHTD, MOVE1, HI, BYE
C
C               VX1(N), VXI(N), VXS(N)
      dimension VX1(*), VXI(*), VXS(*)
C     !EJECT
C
      call HI ('TAMALE')
C     !BEG
      if(IQEXA.gt.0) then
        call NAUGHTD (VXS, 1, N, VZERO)
        if(VZERO) then
          call MOVE1 (VX1, N, VXI)
          call MOVE1 (VX1, N, VXS)
          KVXVA = 1
        end if
      end if
C     !END
      call BYE ('TAMALE')
C
      return
      end
