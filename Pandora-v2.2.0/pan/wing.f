      subroutine WING
     $(ITAU,JUP,KLO,SINGLE,SET,XNU,Z,XINK,FINK,N,NL,R)
C
C     Rudolf Loeser, 1978 Nov 28
C---- Computes R for MESA, for 2-photon and thin transitions.
C     !DASH
      save
C     !DASH
      real*8 DNU, F, FINK, ONE, R, SET, TINC, XINK, XNU, Z
      integer IQINC, ITAU, IUL, JUP, KLO, N, NL
      logical SINGLE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
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
      equivalence (IQQ( 51),IQINC)
C     !DASH
      external INDXUL, SPUME, HI, BYE
C
C               SET(N,MUL), XNU(NSL), Z(N), XINK(INK), FINK(INK)
      dimension SET(N,*),   XNU(*),   Z(*), XINK(*),   FINK(*)
C     !EJECT
C
      call HI ('WING')
C     !BEG
      R = ONE
C
      if(IQINC.gt.0) then
        DNU = XNU(JUP)-XNU(KLO)
C
        if(SINGLE) then
          F = ONE
        else
          call INDXUL (JUP, KLO, IUL)
          F = ONE-SET(ITAU,IUL)
        end if
C
        call SPUME    (DNU, ITAU, Z, 1, 2, XINK, FINK, TINC)
C
        R = R+TINC*F
C
      end if
C     !END
      call BYE ('WING')
C
      return
      end
