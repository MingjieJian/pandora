      subroutine DAISY
     $(N,IU,IL,CWR,CHOP,CHLIM,ILI,M,W,TAU,TRA,KTRAS,RHOJ,RHOP,RHOW,
     $ T,PTAU,WW,WEIT,DUMP,KILROY)
C
C     Rudolf Loeser, 1991 Sep 25
C---- Computes RHOW: "Combination" RHOs.
C     (This is version 2 of DAISY.)
C     !DASH
      save
C     !DASH
      real*8 CHLIM, CHOP, CWR, PTAU, RHOJ, RHOP, RHOW, T, TAU, TRA, W,
     $       WEIT, WW
      integer IL, ILI, IQRWO, IU, KLIN, KMSS, KTRAS, M, MODE, N
      logical DUMP, KILROY
      character qummy*8
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
      equivalence (IQQ(204),IQRWO)
C     !DASH
      external MOVE1, PHLOX, VISE, FARRE, WEITER, HI, BYE
C
C               W(M), TAU(N), TRA(N), RHOJ(N), RHOP(N), RHOW(N), WW(N),
      dimension W(*), TAU(*), TRA(*), RHOJ(*), RHOP(*), RHOW(*), WW(*),
C
C               T(N), PTAU(N), WEIT(N)
     $          T(*), PTAU(*), WEIT(*)
C
      data KLIN,MODE,KMSS /0, 1, 0/
C     !EJECT
C
      call HI ('DAISY')
C     !BEG
C---- Set up T
      if((IQRWO.gt.0).and.(KTRAS.gt.0)) then
        call MOVE1 (TRA, N, T)
      else
        call MOVE1 (TAU, N, T)
      end if
C
C---- Set up weights PTAU
      call PHLOX   (ILI, CHOP, CHLIM, N, T, PTAU)
C---- Compute WW (smoothed PTAU)
      call VISE    (PTAU, N, W, M, WW)
C---- ( ? Dump)
      call FARRE   (N, IU, IL, T, PTAU, WW, DUMP, KILROY)
C
C---- Compute
      call WEITER  (RHOW, RHOJ, RHOP, WW, CWR, N, KLIN, MODE, KMSS,
     $              qummy, WEIT)
C     !END
      call BYE ('DAISY')
C
      return
      end
