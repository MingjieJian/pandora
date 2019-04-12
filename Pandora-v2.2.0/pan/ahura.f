      subroutine AHURA
     $(X,W,IW,Y,WN,WH,ILFLX,ISR,MIK,DL,MPROM,XNE,DP,DW,DDL,FDDL,CDL,
     $ LDL,COP,GTN,IMG,TITLE,LL)
C
C     Rudolf Loeser, 1981 Nov 05
C---- Computes weight matrices using spherical coordinates.
C
C     Returns with
C     MIK=0 if intermediate TAU set seemed OK and matrices were
C              computed; but with
C     MIK=1 if TAU was not monotonic and matrices were not computed.
C
C     Returns with ISR=1 always.
C     !DASH
      save
C     !DASH
      real*8 CDL, COP, CORE, DDL, DL, DP, DW, FDDL, GTN, W, WH, WN, X,
     $       XNE, Y
      integer IKPL, ILFLX, IMG, IN, IOPAC, IOPAX, IPHI, IQTOP, IS, ISR,
     $        ITDSK, ITSHL, IW, JJVXS, JJXDK, JJXSH, K1, KD, KS, LDL,
     $        LL, MIK, MOX, MPROM, N
      logical MOVING, TOP
      character TITLE*100
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
C
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ(129),JJVXS)
      equivalence (IZOQ( 40),JJXSH)
      equivalence (IZOQ(132),JJXDK)
C     !EJECT
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
      equivalence (IQQ(156),IQTOP)
C     !DASH
      external MAZDA, YMUIR, ELSI, DAKINI, DASUNI, MITHRAS, WGIVE,
     $         HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               CDL(LDL), IMG(N), WN(N,N), WH(N,N), DP(N,LDL), DL(K),
      dimension CDL(*),   IMG(*), WN(*),   WH(*),   DP(*),     DL(*),
C
C               DW(N), DDL(LDL), XNE(N), COP(N), GTN(N), FDDL(*)
     $          DW(*), DDL(*),   XNE(*), COP(*), GTN(*), FDDL(*)
C
      dimension IN(6)
      equivalence
     $(IN( 1),ITSHL ),(IN( 2),ITDSK ),(IN( 3),IKPL  ),(IN( 4),IOPAX ),
     $(IN( 5),IOPAC ),(IN( 6),IPHI  )
C
      data MOVING /.false./
      data CORE,K1 /0.D0, 1/
C     !EJECT
C
      call HI ('AHURA')
C     !BEG
C     (Get, and allocate, W allotment)
      call MAZDA     (IN, IS, MOX, 'AHURA')
C
      ISR = 1
      TOP = (IQTOP.gt.0)
C---- Compute line profile (Note: VXS=0)
      call YMUIR     (W, IW, CORE, DL, K1, DP, DW, XNE, X(JJVXS), N,
     $                DDL, FDDL, CDL, LDL, MPROM, W(IPHI))
C---- Compute total opacity
      call ELSI      (1, N, W(IPHI), COP, GTN, W(IKPL), W(IOPAC))
C---- Compute optical depths
      call DAKINI    (W(IOPAC), W(IOPAX), X(JJXSH), TOP, W(ITSHL), KS,
     $                IMG, W)
      call DASUNI    (W(IOPAC),           X(JJXDK), TOP, W(ITDSK), KD,
     $                IMG, W)
C
      if((KS*KD).eq.0) then
        MIK = 1
      else
        MIK = 0
C
C----   Compute matrices
        call MITHRAS (X, W, IW, W(ITSHL), W(ITDSK), Y, MOVING, WN, WH,
     $                ILFLX, TITLE)
      end if
C
C     (Give back W allotment)
      call WGIVE     (W, 'AHURA')
C     !END
      call BYE ('AHURA')
C
      return
      end
