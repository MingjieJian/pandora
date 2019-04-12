      subroutine PEN
     $(ITAU,KUP,JLO,SINGLE,XNU,Z,XINK,FINK,T)
C
C     Rudolf Loeser, 1978 Dec 01
C---- Computes T, for MUSA.
C     !DASH
      save
C     !DASH
      real*8 D, FINK, T, XINK, XNU, Z, ZERO
      integer IQINC, ITAU, JLO, KUP
      logical SINGLE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
      external SPUME, HI, BYE
C
C               XNU(NSL), Z(N), XINK(INK), FINK(INK)
      dimension XNU(*),   Z(*), XINK(*),   FINK(*)
C     !EJECT
C
      call HI ('PEN')
C     !BEG
      if((IQINC.gt.0).and.SINGLE) then
        D = XNU(KUP)-XNU(JLO)
        call SPUME (D,ITAU,Z,1,2,XINK,FINK,T)
      else
        T = ZERO
      end if
C     !END
      call BYE ('PEN')
C
      return
      end
