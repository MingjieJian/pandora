      subroutine MAUVE
     $(KASE)
C
C     Rudolf Loeser, 1980 Nov 08
C---- Picks processing option, for ZIPPY.
C     (This is version 4 of MAUVE.)
C     !DASH
      save
C     !DASH
      integer IQNHA, KASE, KTKIN
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
      equivalence (IQQ( 35),IQNHA)
C     !DASH
      external HI, BYE
C     !EJECT
C
      call HI ('MAUVE')
C     !BEG
      if(KTKIN.gt.0) then
        KASE = 1
      else
        if(IQNHA.gt.0) then
          KASE = 2
        else
          KASE = 3
        end if
      end if
C     !END
      call BYE ('MAUVE')
C
      return
      end
