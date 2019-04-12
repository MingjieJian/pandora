      subroutine BORA
     $(TAU,N,XLM,KODE,II,NR)
C
C     Rudolf Loeser, 1983 Feb 01
C---- Computes reduced table indices, for Continuum Jnu calculation.
C     Returns with KODE = 1 if OK; =0 if not.
C     !DASH
      save
C     !DASH
      real*8 TAU, XLM
      integer II, IQSFS, IQUTM, KODE, N, NR
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
      equivalence (IQQ( 31),IQSFS)
      equivalence (IQQ(321),IQUTM)
C     !DASH
      external AMANA, HI, BYE
C
C               TAU(N)
      dimension TAU(*)
C
      call HI ('BORA')
C     !BEG
      KODE = 1
      II   = 0
      NR   = N
      if((IQUTM.gt.0).and.(IQSFS.le.0)) then
        call AMANA  (TAU,N,XLM,II,KODE)
        NR = (N+1)-(II-1)
      end if
C     !END
      call BYE ('BORA')
C
      return
      end
