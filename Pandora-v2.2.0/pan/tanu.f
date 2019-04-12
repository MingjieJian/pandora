      subroutine TANU
     $(N,X,F,TAU,LABEL,J)
C
C     Rudolf Loeser, 2004 Jan 06
C---- Computes TAU along a ray, by trapezoidal rule.
C
C---- See also TUNA.
C     !DASH
      save
C     !DASH
      real*8 F, HALF, TAU, X, ZERO
      integer I, IQDNT, IQMNT, J, MO, N
      logical lummy
      character LABEL*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
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
      equivalence (IQQ( 64),IQDNT)
      equivalence (IQQ( 54),IQMNT)
C     !DASH
C     !EJECT
      external AORI, IMPROVE, IS_INCREASING, HI, BYE
C
C               X(N), F(N), TAU(N)
      dimension X(*), F(*), TAU(*)
C
      call HI ('TANU')
C     !BEG
C---- Integrate by trapezoidal rule
      TAU(1) = ZERO
      do 100 I = 2,N
        TAU(I) = TAU(I-1) +HALF*(X(I)-X(I-1))*(F(I)+F(I-1))
  100 continue
C
      if((MO.gt.0).and.(IQDNT.gt.0)) then
C       Dump
        call AORI          (X, F, N, TAU, LABEL, 'TANU')
      end if
C
      if(IQMNT.gt.0) then
C----   Attempt to force monotonicity
        call IMPROVE       (TAU, X, N, 'TANU', J, lummy)
      else
C----   Check for monotonicity
        call IS_INCREASING (TAU, 1, N, 0, J)
      end if
C     !END
      call BYE ('TANU')
C
      return
      end
