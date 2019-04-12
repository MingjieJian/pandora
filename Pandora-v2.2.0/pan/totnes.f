      subroutine TOTNES
     $(X,W,IW,XLB1,N,NSHL,NRPMX,CODSRW,MRR,PHISHL,PHIDSK,CKLSHL,CKLDSK,
     $ OPACSHL,OPACDSK,WNSHL,WNDSK,VXS,COP,XLM,DL,XNE,FMULT,INDSHL,
     $ INDDSK,XSHL,XDSK,Z,R1N,EMDSK,WHSHL,WHDSK,ILFLX,Y,MOVING,MPROM,
     $ IMG,DUMP)
C
C     Rudolf Loeser, 1983 Feb 23
C---- Set up comoving profile PHI, line opacity CKL, total opacity OPAC,
C     and weight matrices WN and WH, for all rays, for expanding
C     spherical atmospheres.
C     Keeps track separately of shell components (suffix "SHL") and
C     disk components (suffix "DSK").
C     !DASH
      save
C     !DASH
      real*8 CKLDSK, CKLSHL, CODSRW, COP, DL, EMDSK, FMULT, OPACDSK,
     $       OPACSHL, PHIDSK, PHISHL, R1N, VXS, W, WHDSK, WHSHL, WNDSK,
     $       WNSHL, X, XDSK, XLB1, XLM, XNE, XSHL, Y, Z
      integer ILFLX, IMG, INDDSK, INDSHL, IQTOP, IW, MPROM, MRR, N,
     $        NRPMX, NSHL
      logical DUMP, MOVING, TOPT
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
      equivalence (IQQ(156),IQTOP)
C     !DASH
C     !EJECT
      external GUNBERT, FROLLO, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               CODSRW(NSHL), PHISHL(N,N,NSHL), PHIDSK(N,N,MRR), DL(1),
      dimension CODSRW(*),    PHISHL(*),        PHIDSK(*),       DL(*),
C
C               CKLSHL(N,N,NSHL), CKLDSK(N,N,MRR), XDSK(N,MRR), IMG(N),
     $          CKLSHL(*),        CKLDSK(*),       XDSK(*),     IMG(*),
C
C               OPACDSK(N,N,MRR), WNDSK(N,N,MRR), EMDSK(N,MRR), XNE(N),
     $          OPACDSK(*),       WNDSK(*),       EMDSK(*),     XNE(*),
C
C               OPACSHL(N,N,NSHL), WNSHL(N,N,NSHL), XLB1(Li1len), Z(N),
     $          OPACSHL(*),        WNSHL(*),        XLB1(*),      Z(*),
C
C               XSHL(NRPMX,NSHL), WHSHL(N,N,NSHL), WHDSK(N,N,MRR),
     $          XSHL(*),          WHSHL(*),        WHDSK(*),
C
C               VXS(N), COP(N)
     $          VXS(*), COP(*)
C
      call HI ('TOTNES')
C     !BEG
      TOPT = IQTOP.gt.0
C
      call GUNBERT (X, W, IW, XLB1, N, NSHL, NRPMX, CODSRW, PHISHL,
     $              CKLSHL, OPACSHL, WNSHL, VXS, COP, XLM, DL, XNE,
     $              MPROM, FMULT, INDSHL, TOPT, XSHL, Z, R1N, WHSHL,
     $              Y, MOVING, ILFLX, IMG, DUMP)
C
      call FROLLO  (X, W, IW, XLB1, N, MRR, PHIDSK, CKLDSK, OPACDSK,
     $              WNDSK, VXS, COP, XLM, DL, XNE, MPROM, FMULT,
     $              INDDSK, TOPT, XDSK, EMDSK, WHDSK, Y, MOVING,
     $              ILFLX, IMG, DUMP)
C     !END
      call BYE ('TOTNES')
C
      return
      end
