      subroutine ANGARAD
     $(N,NSHL,NRPMX,CODSRW,MRR,PHISHL,PHIDSK,WNSHL,WNDSK,SHLKOD,DSKKOD,
     $ VXS,GTN,COP,DP,DW,MPROM,XNE,DDL,FDDL,CDL,LDL,XSHL,XDSK,Z,R1N,
     $ EMDSK,WAVE,DL,LL,IND,WHSHL,WHDSK,ILFLX,Y,MOVING,IMG,W,IW,DUMP)
C
C     Rudolf Loeser, 1983 Mar 02
C---- Sets up comoving profile, PHI, weight matrices, WN and WH, and
C     matrix signals, KODE, for all rays for an expanding spherical
C     atmosphere. Keeps track separately of shell components
C     (suffix "SHL") and disk components (suffix "DSK").
C     !DASH
      save
C     !DASH
      real*8 CDL, CODSRW, COP, DDL, DL, DP, DSKKOD, DW, EMDSK, FDDL,
     $       GTN, PHIDSK, PHISHL, R1N, SHLKOD, VXS, W, WAVE, WHDSK,
     $       WHSHL, WNDSK, WNSHL, XDSK, XNE, XSHL, Y, Z
      integer ILFLX, IMG, IND, IQTOP, IW, LDL, LL, MPROM, MRR, N, NRPMX,
     $        NSHL
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
      external GILDAS, RAGAN, HI, BYE
C
      dimension W(*), IW(*)
C
C               NRM = 2*N + 5
C
C               WNSHL(N,N,NSHL), PHIDSK(N,N,MRR), WHDSK(N,N,MRR), Z(N),
      dimension WNSHL(*),        PHIDSK(*),       WHDSK(*),       Z(*),
C
C               GTN(N), COP(N), XDSK(N,MRR), FDDL(N), XNE(N), CDL(LDL),
     $          GTN(*), COP(*), XDSK(*),     FDDL(*), XNE(*), CDL(*),
C
C               EMDSK(N,MRR), WHSHL(N,N,NSHL), PHSIHL(N,N,NSHL), DL(1),
     $          EMDSK(*),     WHSHL(*),        PHISHL(*),        DL(*),
C
C               XSHL(NRM,NSHL), CODSRW(NSHL), VXS(N), IMG(N), DDL(LDL),
     $          XSHL(*),        CODSRW(*),    VXS(*), IMG(*), DDL(*),
C
C               WNDSK(N,N,MRR), SHLKOD(NSHL), DSKKOD(MRR), DP(N,LDL),
     $          WNDSK(*),       SHLKOD(*),    DSKKOD(*),   DP(*),
C
C               DW(N)
     $          DW(*)
C
      call HI ('ANGARAD')
C     !BEG
      TOPT = IQTOP.gt.0
C
      call GILDAS (N, VXS, GTN, COP, DP, DW, MPROM, XNE, DDL, FDDL,
     $             CDL, LDL, XSHL, Z, R1N, WAVE, DL, LL, NSHL, NRPMX,
     $             CODSRW, IND, TOPT, PHISHL, WNSHL, WHSHL, ILFLX, Y,
     $             MOVING, SHLKOD, IMG, W, IW, DUMP)
C
      call RAGAN  (N, VXS, GTN, COP, DP, DW, MPROM, XNE, DDL, FDDL,
     $             CDL, LDL, EMDSK, XDSK, WAVE, DL, LL, MRR, IND, TOPT,
     $             PHIDSK, WNDSK, WHDSK, ILFLX, Y, MOVING, DSKKOD, IMG,
     $             W, IW, DUMP)
C     !END
      call BYE ('ANGARAD')
C
      return
      end
