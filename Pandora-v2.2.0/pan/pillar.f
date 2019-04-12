      subroutine PILLAR
     $(LU)
C
C     Rudolf Loeser, 1981 Oct 27
C---- Sets up output unit for OSTYAK.
C     !DASH
      save
C     !DASH
      integer IOVER, IQPSG, IQSTA, LU, MO, NO
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
      equivalence (LEST( 2),IOVER)
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
      equivalence (IQQ(157),IQPSG)
      equivalence (IQQ( 76),IQSTA)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 5),NO   )
      equivalence (LUNITS( 8),MO   )
C     !DASH
      external HI, BYE
C     !EJECT
C
      call HI ('PILLAR')
C     !BEG
      LU = 0
      if(IOVER.eq.0) then
        if((IQPSG.gt.0).or.(IQSTA.gt.0)) then
          LU = NO
        end if
      else
        if(IQPSG.gt.0) then
          LU = MO
        end if
      end if
C     !END
      call BYE ('PILLAR')
C
      return
      end
