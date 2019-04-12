      subroutine COST
     $(X,RKI,RLI,LU)
C
C     Rudolf Loeser, 2004 Dec 09
C---- Artificial RK enhancement.
C     !DASH
      save
C     !DASH
      real*8 RKI, RLI, X
      integer IQRKE, JJRKX, LU, N, NSL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ(40),NSL)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(  6),JJRKX)
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
      equivalence (IQQ(334),IQRKE)
C     !DASH
C     !EJECT
      external  ABJECT, LOST, HI, BYE
      intrinsic min
C
      dimension X(*)
C
C               RKI(N,NSL), RLI(N,NSL)
      dimension RKI(N,*),   RLI(N,*)
C
      call HI ('COST')
C     !BEG
      if(IQRKE.gt.0) then
        if(LU.gt.0) then
          call ABJECT (LU)
          write (LU,100)
  100     format(' ','Artificial enhancement of RK.')
        end if
C
        call LOST     (N, NSL, X(JJRKX), RKI, RLI, LU)
      end if
C     !END
      call BYE ('COST')
C
      return
      end
