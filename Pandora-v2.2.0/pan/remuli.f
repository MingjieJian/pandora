      subroutine REMULI
     $(N,IU,IL,X,XNU,CNDT)
C
C     Rudolf Loeser, 2004 May 17
C---- Computes incident radiation term, CNDT, for LEMON.
C     (This is version 2 of REMULI.)
C     !DASH
      save
C     !DASH
      real*8 CNDT, DNU, X, XNU
      integer IL, IQINC, IU, N
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
      equivalence (IQQ( 51),IQINC)
C     !DASH
      external ZERO1, SPUMONI, HI, BYE
C
      dimension X(*)
C
C               XNU(NSL), CNDT(N)
      dimension XNU(*),   CNDT(*)
C
      call HI ('REMULI')
C     !BEG
      if(IQINC.gt.0) then
        DNU = XNU(IU)-XNU(IL)
        call SPUMONI (DNU, 1, X, CNDT)
      else
        call ZERO1   (CNDT, N)
      end if
C     !END
      call BYE ('REMULI')
C
      return
      end
