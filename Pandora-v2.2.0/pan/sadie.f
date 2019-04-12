      subroutine SADIE
     $(N,IU,IL,XNU,SET,SR,B,SOB,XVAL,W,IW,S)
C
C     Rudolf Loeser, 2004 May 19
C---- Computes S(IU/IL) directly.
C     (This is version 3 of SADIE.)
C     !DASH
      save
C     !DASH
      real*8 B, S, SET, SOB, SR, W, XNU, XVAL
      integer IL, IQSSM, IU, IW, N
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
      equivalence (IQQ(310),IQSSM)
C     !DASH
      external VERNAL, AMALRIC, MOVE1, MURIEL, HI, BYE
C
      dimension W(*), IW(*)
C
C               SET(N,MUL), S(N), B(N), SR(N), XVAL(N), SOB(N), XNU(NSL)
      dimension SET(N,*),   S(*), B(*), SR(*), XVAL(*), SOB(*), XNU(*)
C     !EJECT
C
      call HI ('SADIE')
C     !BEG
C---- Compute "raw" S
      call MURIEL    (N, IU, IL, XNU, SET, SR)
C
      if(IQSSM.gt.0) then
C----   Obtain S, the "final" S via smoothing the curve of log(SR/B)
        call AMALRIC (SR, B, S, N, IU, IL, SOB, XVAL, W, IW)
      else
C----   Just set "final" equal to "raw"
        call MOVE1   (SR, N, S)
      end if
C
C---- (Dump ?)
      call VERNAL    (N, IU, IL, XNU, SET, SR, B, S)
C     !END
      call BYE ('SADIE')
C
      return
      end
