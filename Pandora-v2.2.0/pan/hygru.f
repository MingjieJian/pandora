      subroutine HYGRU
     $(CVXS,CVSB,NVX,CVX,DOFMV)
C
C     Rudolf Loeser, 1993 Dec 20
C---- Determines whether FMV needs to be computed.
C     !DASH
      save
C     !DASH
      real*8 CVSB, CVX, CVXS, ONE, SUM, ZERO
      integer I, ION, IQAMD, IQFLW, IQVLG, NVX
      logical DOFMV, OPT
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
      equivalence (IQQ(219),IQAMD)
      equivalence (IQQ(221),IQVLG)
      equivalence (IQQ(335),IQFLW)
C     !DASH
      external  PENDA, HI, BYE
      intrinsic abs
C
C               CVX(NVX)
      dimension CVX(*)
C     !EJECT
C
      call HI ('HYGRU')
C     !BEG
      SUM = ZERO
C
      call PENDA (ION)
      if(ION.ne.0) then
        OPT = (IQAMD.gt.0).or.(IQVLG.gt.0).or.(IQFLW.gt.0)
        if(OPT) then
          SUM = SUM+ONE
        end if
      end if
      SUM = SUM+abs(CVXS)+abs(CVSB)
C
      if(NVX.gt.0) then
        do 100 I = 1,NVX
          SUM = SUM+abs(CVX(I))
  100   continue
      end if
C
      DOFMV = SUM.ne.ZERO
C     !END
      call BYE ('HYGRU')
C
      return
      end
