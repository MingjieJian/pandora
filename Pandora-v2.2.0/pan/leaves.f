      subroutine LEAVES
     $(N,MRR,K,IU,IL,DUMP,R1N,Z,FRR,COPTRN,BCTRN,GTN,STRN,DP,DW,XNE,
     $ MPROM,DDL,FDDL,CDL,LDL,VEX,V,VR,TE,DL,SI,INT1S,TAU1S,DI,
     $ INT1D,TAU1D,W,IW)
C
C     Rudolf Loeser, 2000 Jun 20
C---- Computes Line Intensity Profiles, both Shell (SI) and Disk (DI).
C     (This is version 5 of LEAVES.)
C     !DASH
      save
C     !DASH
      real*8 BCTRN, CDL, COPTRN, DDL, DI, DL, DP, DW, FDDL, FRR, GTN,
     $       R1N, SI, STRN, TAU1D, TAU1S, TE, V, VEX, VR, W, XNE, Z,
     $       ZERO, ZN
      integer I, IDX, IFX, IGX, IL, IN, INT1D, INT1S, IQVSW, IRT, IS,
     $        ISUL, ITX, IU, IW, IZT, J, K, LDL, MOX, MPROM, MRR, N
      logical DISK, DMPI, DUMP, VSW
      character LABEL*100
C     !COM
C---- LOOPER      as of 2006 May 05
      integer     NVEL,NVY,JVEL,LFBV,LFB,MF,MUK
      real*8      EMOO,WVLTRN
      logical     EXPAND,SPHERE,VXZERO,FLOBRD
      common      /LOOPER1/ NVEL,NVY,JVEL,LFBV,LFB,MF,MUK
      common      /LOOPER2/ EMOO,WVLTRN
      common      /LOOPER3/ EXPAND,SPHERE,VXZERO,FLOBRD
C
C     Emergent Profiles calculations control data (simplified version).
C
C     NVEL   : number of velocity tables
C     NVY    : current value of velocity-loop index, 1.le.NVY.le.NVEL
C              (i.e. index of current velocity set)
C     JVEL   : code describing current velocity set (i.e. KVEL(NVY) )
C              =     1 : VSB
C              =     2 : VXS
C              =     3 : VADD     (from AMDIFF and/or VELGRAD)
C              = 100+j : VXN(j)
C              = 200+j : VXN(j) + VADD
C              = 300+j : VAX(j)
C              = 400+j : VFB(j)
C              = 500+j : VFB(j) + VADD
C
C     LFBV   : number of viewing positions (front only, or back also)
C     LFB    : current value of views-loop index, 1 .le. LFB .le. LFBV
C              = 1 - front-face
C              = 2 - back-face
C
C     MF     : current value of look-angles-loop index, 1.le.MF.le.LF
C     MUK    : is .gt. 0 if line intensity profile must be printed
C              [when MUK > 0, then EMOO = EMU(MUK) ]
C     EMOO   : current value of look-angle
C     WVLTRN : wavelength (Angstroms) (i.e. at Delta-Lambda = 0).
C
C     VXZERO : tells whether the current velocity =0, or not
C     EXPAND : tells whether the procedures for expanding atmospheres
C              should be used (set up in SWEET)
C     SPHERE : tells whether this is a spherical, as opposed to
C              plane-parallel, atmosphere
C     FLOBRD : tells whether to use the flow-broadening procedure
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
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
      equivalence (IQQ(173),IQVSW)
C     !DASH
C     !EJECT
      external LEDA, GYRE, GOOD, CASTOR, POLLUX, WGIVE, MESHED, MASHED,
     $         HI, BYE
C
      dimension W(*), IW(*)
C
C               Z(N), DDL(LDLMX), INT1S(N,KM), DI(MRR,KM), BCTRN(N,KM),
      dimension Z(*), DDL(*),     INT1S(N,*),  DI(MRR,*),  BCTRN(*),
C
C               GTN(N), STRN(N,KM), VEX(N), XNE(N), FRR(MRR), SI(N,KM),
     $          GTN(*), STRN(*),    VEX(*), XNE(*), FRR(*),   SI(N,*),
C
C               FDDL(N), TE(N), VR(N), TAU1S(N,KM), DL(KM), CDL(LDLMX),
     $          FDDL(*), TE(*), VR(*), TAU1S(N,*),  DL(*),  CDL(*),
C
C               TAU1D(MRR,KM), COPTRN(N,KM), DW(N), DP(N,LDLMX), V(N),
     $          TAU1D(MRR,*),  COPTRN(*),    DW(*), DP(*),       V(*),
C
C               INT1D(MRR,KM)
     $          INT1D(MRR,*)
C
      dimension IN(5)
      equivalence
     $(IN( 1),ISUL  ),(IN( 2),IDX   ),(IN( 3),IFX   ),(IN( 4),IGX   ),
     $(IN( 5),ITX   )
C
      call HI ('LEAVES')
C     !BEG
C     (Get, and allocate, W allotment)
      call LEDA         (IN, IS, MOX, 'LEAVES')
C
      DISK = MRR.gt.0
      VSW  = IQVSW.gt.0
      ZN   = Z(N)
      if(DUMP) then
        IZT = N/2
        IRT = MRR/2
        call MESHED     ('LEAVES', 2)
        call GYRE       (IU, IL, R1N, Z, BCTRN, GTN, COPTRN, STRN, DP,
     $                   DW, N, K)
      end if
C     !EJECT
C---- Loop over all Delta-Lambda points of the profiles
      do 102 J = 1,K
C----   Get Shell Intensity
        SI(1,J)    = ZERO
        INT1S(1,J) = 0
        TAU1S(1,J) = ZERO
        do 100 I = 2,N
          call GOOD     (DUMP, 1, DL(J), I, IZT, LABEL, DMPI)
          call CASTOR   (N, I, J, K, R1N, WVLTRN, DL, DMPI, ZN, Z, DP,
     $                   DW, XNE, MPROM, DDL, FDDL, CDL, LDL, VEX, V,
     $                   VR, TE, GTN, W(ISUL), COPTRN, BCTRN, W(IDX),
     $                   W(IFX), W(IGX), W(ITX), SI(I,J), INT1S(I,J),
     $                   TAU1S(I,J), STRN, EXPAND, LABEL, VSW, W, IW)
  100   continue
C
        if(DISK) then
C----     Get Disk Intensity
          do 101 I = 1,MRR
            call GOOD   (DUMP, 2, DL(J), I, IRT, LABEL, DMPI)
            call POLLUX (N, I, J, K, R1N, WVLTRN, DL, DMPI, FRR(I), Z,
     $                   DP, DW, XNE, MPROM, DDL, FDDL, CDL, LDL, VEX,
     $                   V, VR, TE, GTN, W(ISUL), COPTRN, BCTRN,
     $                   W(IDX), W(IFX), W(IGX), W(ITX), DI(I,J),
     $                   INT1D(I,J), TAU1D(I,J), STRN, EXPAND, LABEL,
     $                   VSW, W, IW)
  101     continue
        end if
  102 continue
      if(DUMP) then
        call MASHED     ('LEAVES')
      end if
C
C     (Give back W allotment)
      call WGIVE        (W, 'LEAVES')
C     !END
      call BYE ('LEAVES')
C
      return
      end
