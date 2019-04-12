      subroutine FRATE
     $(X,VXN,N,CVX,ISSV,WT,VEC,FR,VXI,VXS,WTP)
C
C     Rudolf Loeser, 1988 Aug 10
C---- Generates tables of VXN values:
C               additional expansion velocities, or
C               shock velocities, or
C               flow-broadening velocities (and WTP).
C     !DASH
      save
C     !DASH
      real*8 CVX, FR, VEC, VXI, VXN, VXS, WT, WTP, X
      integer IQFLW, ISSV, J, JJFMV, JJHEA, JJHND, JJZ, N, NFH, NVX
      logical CALVAX, FLWBRD, KILROY
      character qummy*8
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(42),NVX)
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 37),JJZ  )
      equivalence (IZOQ( 11),JJHND)
      equivalence (IZOQ(219),JJHEA)
      equivalence (IZOQ(225),JJFMV)
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
      equivalence (IQQ(335),IQFLW)
C     !DASH
C     !EJECT
      external KALMIA, FETRA, TAMALE, RAFTER, BUSKER, HI, BYE
C
      dimension X(*)
C
C               CVX(NVX), VXN(N,NVX), ISSV(NVX), VXI(N), VXS(N), FR(N),
      dimension CVX(*),   VXN(N,*),   ISSV(*),   VXI(*), VXS(*), FR(*),
C
C               VEC(NFH), WT(NVX), WTP(NVX)
     $          VEC(*),   WT(*),   WTP(*)
C
      call HI ('FRATE')
C     !BEG
C---- Check input
      call RAFTER       (NVX, CVX, ISSV)
C
      if(NVX.gt.0) then
        CALVAX = ISSV(1).gt.0
        FLWBRD = IQFLW.gt.0
        KILROY = .true.
C
        if(CALVAX) then
C----     Set up shock velocities
          do 100 J = 1,NVX
            call FETRA  (N, ISSV(J), X(JJZ), VXN(1,J))
  100     continue
        else if(FLWBRD) then
C----     Set up flow-broadening velocities ( ABORT  on error)
          call BUSKER   (X, VXN, N, CVX, WT, WTP, VEC, FR, KILROY)
        else
C----     "Additional" expansion velocities
          do 101 J = 1,NVX
            call KALMIA (N, CVX(J), X(JJZ), X(JJHND), X(JJHEA), FR,
     $                   X(JJFMV), KILROY, qummy, 0, VXN(1,J))
  101     continue
        end if
C
C----   Set up VXS (if indicated)
        call TAMALE     (N, VXN(1,1), VXI, VXS)
C
      end if
C     !END
      call BYE ('FRATE')
C
      return
      end
