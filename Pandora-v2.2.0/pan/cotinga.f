      subroutine COTINGA
     $(LU,LUN,LUD,LUF,LUP,LUE,LUH)
C
C     Rudolf Loeser, 2003 Jun 18
C---- Sets up output units for number density printout, and
C     prints header and explanation.
C     (This is version 4 of COTINGA.)
C     !DASH
      save
C     !DASH
      integer IHSLT, IQBNV, IQNPL, IQNUM, IQSNB, ITHSL, LU, LUD, LUE,
     $        LUF, LUH, LUN, LUP
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
      equivalence (KZQ( 20),IHSLT)
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(19),ITHSL)
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
      equivalence (IQQ( 29),IQNUM)
      equivalence (IQQ(118),IQSNB)
      equivalence (IQQ( 34),IQNPL)
      equivalence (IQQ(325),IQBNV)
C     !DASH
C     !EJECT
      external  ZEUS, PRIAM, ESNI, HI, BYE
      intrinsic max
C
      call HI ('COTINGA')
C     !BEG
      LUN = 0
      LUD = 0
      LUF = 0
      LUP = 0
      LUE = 0
      if(IHSLT.eq.ITHSL) then
        call ZEUS (LU , IQNUM, LUN)
        call ZEUS (LUN, IQSNB, LUF)
        call ZEUS (LUN, IQBNV, LUD)
        call ZEUS (LU , IQNPL, LUP)
        LUE = LUN
      end if
      LUH = max(LUN, LUF, LUP, LUE, LUD)
C
C---- Print header
      call PRIAM  (LU, 'POPULATIONS', 11)
C---- Print explanations
      call ESNI   (LU)
C
C     !END
      call BYE ('COTINGA')
C
      return
      end
