      subroutine DRONGO
     $(N,TAU,DP,DW,RHO)
C
C     Rudolf Loeser, 1985 Apr 16
C---- Sets up values of RHO according to the Escape Probability
C     approximation to the solution of the static transfer equation.
C     !DASH
      save
C     !DASH
      real*8 DP, DW, REAR, RHO, TAU
      integer I, IQFIN, N
      logical FIN
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
      equivalence (IQQ( 49),IQFIN)
C     !DASH
      external FRIGATE, HI, BYE
C
C               TAU(N), RHO(N), DP(N), DW(N,LDL)
      dimension TAU(*), RHO(*), DP(*), DW(N,*)
C     !EJECT
C
      call HI ('DRONGO')
C     !BEG
      FIN = IQFIN.gt.0
C
      do 100 I = 1,N
        call FRIGATE   (TAU(I),DP(I),DW(I,1),RHO(I))
        if(FIN) then
          call FRIGATE ((TAU(N)-TAU(I)),DP(I),DW(I,1),REAR)
          RHO(I) = RHO(I)+REAR
        end if
  100 continue
C
      RHO(1) = RHO(2)
C
      if(FIN) then
        RHO(N) = RHO(N-1)
      end if
C     !END
      call BYE ('DRONGO')
C
      return
      end
