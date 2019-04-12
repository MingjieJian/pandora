      subroutine TUNA
     $(N,X,F,TAU,LABEL,J,EDINT,EDTAU,IMG,W)
C
C     Rudolf Loeser, 1978 Sep 06
C---- Computes and checks an optical depth scale, using an elaborate
C     parabolic scheme.
C
C     Upon return, J .eq. 0 if the TAU values are nondescending,
C     .gt. 0 if not. If J .gt. 0, then its value is the index of the
C     first nonmonotonic value of TAU.
C
C     Also, EDINT = true if the integrand was edited before integration,
C     and EDTAU = true if TAU-editing was attempted.
C
C     (This is version 3 of TUNA.)
C
C---- See also TANU.
C     !DASH
      save
C     !DASH
      real*8 F, TAU, W, X
      integer IMG, IQDNT, IQMNT, J, MO, N
      logical DUMP, EDINT, EDTAU
      character LABEL*(*)
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
      equivalence (IQQ( 64),IQDNT)
      equivalence (IQQ( 54),IQMNT)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !DASH
C     !EJECT
      external AUNT, PAMPAS, IMPROVE, IS_INCREASING, HI, BYE
C
      dimension W(*)
C
C               X(N), TAU(N), F(N), IMG(N)
      dimension X(*), TAU(*), F(*), IMG(*)
C
      call HI ('TUNA')
C     !BEG
C---- Check for negatives in integrand
      call AUNT            (N, F, LABEL, EDINT, IMG, W)
C
C---- Integrate
      DUMP = (IQDNT.gt.0).and.(MO.gt.0)
      call PAMPAS          (N, X, F, TAU, LABEL, DUMP, W)
C
      if(IQMNT.gt.0) then
C----   Attempt to force monotonicity
        call IMPROVE       (TAU, X, N, 'TUNA', J, EDTAU)
      else
C----   Check monotonicity
        call IS_INCREASING (TAU, 1, N, 0, J)
      end if
C     !END
      call BYE ('TUNA')
C
      return
      end
