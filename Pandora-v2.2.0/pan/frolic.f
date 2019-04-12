      subroutine FROLIC
     $(X,IX,W,IW,XPBL,RKI,IQRK,RLI,IQRL,LUP,LUG,LUS)
C
C     Rudolf Loeser, 1978 Dec 28
C---- Computes and prints RK and RL, for SETTUP.
C     !DASH
      save
C     !DASH
      real*8 RKI, RLI, W, X, XPBL
      integer IQOTL, IQRK, IQRL, IQUTR, IW, IX, LUG, LUP, LUS, N, NSL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(40),NSL)
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
      equivalence (IQQ( 53),IQUTR)
      equivalence (IQQ(344),IQOTL)
C     !DASH
C     !EJECT
      external ZERO1, RINA, RATES, GOAT, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               RKI(N,NSL), RLI(N,NSL), XPBL(Lenpbl), IQRK(NSL),
      dimension RKI(*),     RLI(*),     XPBL(*),      IQRK(*),
C
C               IQRL(NSL)
     $          IQRL(*)
C
      call HI ('FROLIC')
C     !BEG
      if(IQOTL.gt.0) then
C----   Set RKI = 0 for optically-thin limit
        call ZERO1 (RKI, (N*NSL))
      end if
      if(IQUTR.le.0) then
C----   RK and RL by integration over continua
        call RINA  (X, IX, W, RKI, IQRK, RLI, IQRL, LUP, LUG, LUS)
      else
C----   RK and RL from radiation temperatures
        call RATES (X, W, RKI, IQRK, RLI, IQRL, LUP)
      end if
C---- Additional effects
      call GOAT    (X, IX, W, IW, XPBL, RKI, IQRK, RLI, IQRL, LUP)
C     !END
      call BYE ('FROLIC')
C
      return
      end
