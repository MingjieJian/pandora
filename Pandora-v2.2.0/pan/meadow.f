      subroutine MEADOW
     $(X,IX,W,IW,RKI,IQRK,RLI,IQRL,XPBL,NO)
C
C     Rudolf Loeser, 1991 Feb 15
C---- Supervises the calculation of the charge exchange terms
C     affecting RK and RL.
C     (This is version 2 of MEADOW.)
C     !DASH
      save
C     !DASH
      real*8 RKI, RLI, W, X, XPBL
      integer IESG, IN, IQCXL, IQCXU, IQRK, IQRL, IS, IW, IX, MCXK, MOX,
     $        NO
      logical KILROY
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
      equivalence (LEST(55),MCXK )
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
      equivalence (IQQ(330),IQCXL)
      equivalence (IQQ(331),IQCXU)
C     !DASH
C     !EJECT
      external TUFF, BUFF, MULE, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               RKI(N,NSL), RLI(N,NSL), IQRK(NSL), IQRL(NSL),
      dimension RKI(*),     RLI(*),     IQRK(*),   IQRL(*),
C
C               XPBL(Lenpbl)
     $          XPBL(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IESG  )
C
      call HI ('MEADOW')
C     !BEG
C     (Get and allocate W allotment)
      call MULE   (IN, IS, MOX, 'MEADOW')
C
      KILROY = .true.
C
      if(IQCXL.gt.0) then
C----   Lower-level charge exchange
        call TUFF (X, IX, W, IW, RKI, IQRK, RLI, IQRL, NO, XPBL,
     $             W(IESG), KILROY)
      end if
C
      if((IQCXU.gt.0).and.(MCXK.ne.0)) then
C----   Upper-level charge exchange
        call BUFF (X, IX,        RKI, IQRK, RLI, IQRL, NO, XPBL,
     $             W(IESG), KILROY)
      end if
C
C     (Give back W allotment)
      call WGIVE  (W, 'MEADOW')
C     !END
      call BYE ('MEADOW')
C
      return
      end
