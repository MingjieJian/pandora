      subroutine CROTON
     $(NOLD,N,INTZ)
C
C     Rudolf Loeser, 1981 Jun 25
C---- Returns with INTZ = 1 if Z-interpolation of
C     PRD input-Jnu's is permitted, = 0 if not.
C     !DASH
      save
C     !DASH
      integer INTZ, IQJNT, KTKIN, N, NOLD
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
      equivalence (LEST(11),KTKIN)
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
      equivalence (IQQ(152),IQJNT)
C     !DASH
      external HI, BYE
C
      call HI ('CROTON')
C     !BEG
      INTZ = 1
      if((N.eq.NOLD).and.(KTKIN.gt.0).and.(IQJNT.le.0)) then
        INTZ = 0
      end if
C     !END
      call BYE ('CROTON')
C
      return
      end
