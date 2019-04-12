      subroutine SARONG
     $(X,W,IW,LL,N,DLLL,XILL,MPROM,XNE,DPUL,DWUL,DDLUL,FDDLUL,CDLUL,
     $ PHI,COPUL,GTNUL,Z,Y,WN,WH,ILFLX,ISR,MIK,IMG,TITLE,DMP1)
C
C     Rudolf Loeser, 1981 May 07
C---- Computes GR-weight matrices for Line Source Functions.
C     (This is version 4 of SARONG.)
C     !DASH
      save
C     !DASH
      real*8 CDLUL, COPUL, DDLUL, DLLL, DPUL, DWUL, FDDLUL, GTNUL, PHI,
     $       W, WH, WN, X, XILL, XNE, Y, Z
      integer ILFLX, IMG, IN, IS, ISR, ITAU, ITMU, IW, JJVXS, JJXMU, LG,
     $        LL, MIK, MOX, MPROM, N
      logical DMP1, MOVING
      character TITLE*100
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ(34),LG )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(129),JJVXS)
      equivalence (IZOQ(130),JJXMU)
C     !DASH
      external MAESTRO, CARAMAN, CRONOS, WGIVE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               CDLUL(LDL), DPUL(N,LDL), GTNUL(N), DDLUL(LDL), DWUL(N),
      dimension CDLUL(*),   DPUL(*),     GTNUL(*), DDLUL(*),   DWUL(*),
C
C               WN(N,N), WH(N,N), FDDLUL(N), PHI(N), DLLL(1), COPUL(N),
     $          WN(*),   WH(*),   FDDLUL(*), PHI(*), DLLL(*), COPUL(*),
C
C               XNE(N), Z(N), IMG(N)
     $          XNE(*), Z(*), IMG(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),ITMU  ),(IN( 2),ITAU  )
C     !EJECT
C
      call HI ('SARONG')
C     !BEG
C     (Get, and allocate, W allotment)
      call MAESTRO  (IN, IS, MOX, 'SARONG')
C
      MOVING = .false.
C
C---- Compute TMUs
      call CARAMAN  (X, W, IW, N, DLLL, X(JJVXS), MPROM, XNE, DPUL,
     $               DWUL, DDLUL, FDDLUL, CDLUL, PHI, XILL, COPUL,
     $               GTNUL, LL, X(JJXMU), LG, W(ITAU), W(ITMU), IMG,
     $               MIK, DMP1)
C
      if(MIK.le.0) then
C----   Compute matrices
        call CRONOS (X, W, IW, W(ITMU), Z, N, Y, MOVING, WN, WH, ILFLX,
     $               TITLE)
        ISR = 1
      else
C----   Set error signal
        ISR = 0
      end if
C
C     (Give back W allotment)
      call WGIVE    (W, 'SARONG')
C     !END
      call BYE ('SARONG')
C
      return
      end
