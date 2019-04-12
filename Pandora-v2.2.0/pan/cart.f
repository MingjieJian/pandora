      subroutine CART
     $(EP1,OE1,EP2,OE2,WEP,N,KMSS,ENW,WEIT)
C
C     Rudolf Loeser, 1981 Dec 11
C---- Supervises Epsilon weighting, for WEAPON.
C     !DASH
      save
C     !DASH
      real*8 ENW, EP1, EP2, OE1, OE2, ONE, WEIT, WEP
      integer IQWSE, KMSS, N
      logical ZEP
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
      equivalence (IQQ(124),IQWSE)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external SET1, NAUGHTD, MOVE1, WEITING, HI, BYE
C
C               EP1(N), OE1(N), EP2(N), OE2(N), ENW(N), WEIT(N)
      dimension EP1(*), OE1(*), EP2(*), OE2(*), ENW(*), WEIT(*)
C     !EJECT
C
      call HI ('CART')
C     !BEG
      if(WEP.lt.ONE) then
C
C----   Set up weights vs. depth
        call SET1      (WEIT,N,WEP)
C
        call NAUGHTD   (OE1,1,N,ZEP)
        if(.not.ZEP) then
C----     Compute weighted EP1
          call MOVE1   (EP1,N,ENW)
          call WEITING (OE1,ENW,EP1,N,WEIT,IQWSE,KMSS,'EP1')
        end if
C
        call NAUGHTD   (OE2,1,N,ZEP)
        if(.not.ZEP) then
C----     Compute weighted EP2
          call MOVE1   (EP2,N,ENW)
          call WEITING (OE2,ENW,EP2,N,WEIT,IQWSE,KMSS,'EP2')
        end if
C
      end if
C     !END
      call BYE ('CART')
C
      return
      end
