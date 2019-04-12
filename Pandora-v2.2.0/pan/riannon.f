      subroutine RIANNON
     $(VSW,DL,K,N,Z,TE,V,VR,VEX,DP,DW,XNE,MPROM,DWU,PHI,LDL,DDL,FDDL,
     $ CDL,W,IW,LUA)
C
C     Rudolf Loeser, 1983 Dec 07
C---- Computes PHI, for GREY.
C     Recomputes DW, and produces a profile analysis, as needed.
C     !DASH
      save
C     !DASH
      real*8 CDL, DDL, DL, DP, DW, DWU, FDDL, PHI, TE, V, VEX, VR, W,
     $       XNE, Z
      integer IDWIN, IEMU, IN, IS, IVXP, IW, K, LDL, LUA, MOX, MPROM, N
      logical DUMP, VSW
      character LABEL*14
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
C     !EJECT
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 75),IDWIN)
C     !DASH
C     !EJECT
      external MORGAN, SET1, TALUS, YMUIR, MOVE1, DERBY, BARAMA, WGIVE,
     $         OCELLI, HI, BYE
C
      dimension W(*), IW(*)
C
C               DL(K), DDL(LDLMX), VR(N), PHI(N,KM), CDL(LDLMX), TE(N),
      dimension DL(*), DDL(*),     VR(*), PHI(*),    CDL(*),     TE(*),
C
C               DP(N,LDLMX), DW(N), DWU(N), XNE(N), VEX(N), Z(N), V(N),
     $          DP(*),       DW(*), DWU(*), XNE(*), VEX(*), Z(*), V(*),
C
C               FDDL(N)
     $          FDDL(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IEMU  ),(IN( 2),IVXP  )
C
      data LABEL /'Plane-parallel'/
C     !EJECT
C
      call HI ('RIANNON')
C     !BEG
C     (Get, and allocate, W allotment)
      call MORGAN     (IN, IS, MOX, 'RIANNON')
C
      if(VSW) then
C----   Compute current DW
        call SET1     (W(IEMU), N, EMOO)
        call TALUS    (TE, V, VR, W(IEMU), N, WVLTRN, 1, DWU)
C----   Compute PHI
        call DERBY    (VEX, W(IEMU), 0, N, W(IVXP))
        call YMUIR    (W, IW, WVLTRN, DL, K, DP, DWU, XNE, W(IVXP), N,
     $                 DDL, FDDL, CDL, LDL, MPROM, PHI)
        DUMP = IDWIN.gt.0
        if(DUMP) then
          call OCELLI (N, Z, TE, V, VR, W(IVXP), W(IEMU), DWU, DP,
     $                 PHI, LABEL)
        end if
      else
C----   Set up current DW
        call MOVE1    (DW, N, DWU)
        if(EXPAND.or. (MF.eq.1)) then
C----     Compute PHI
          call SET1   (W(IEMU), N, EMOO)
          call DERBY  (VEX, W(IEMU), 0, N, W(IVXP))
          call YMUIR  (W, IW, WVLTRN, DL, K, DP, DWU, XNE, W(IVXP), N,
     $                 DDL, FDDL, CDL, LDL, MPROM, PHI)
        end if
      end if
C
      if(MUK.gt.0) then
C----   Print profile analysis
        call BARAMA   (LUA, DL, K, N, Z, DWU, DP, W(IVXP), XNE, MPROM,
     $                 LDL, DDL, FDDL, CDL, W, IW)
      end if
C
C     (Give back W allotment)
      call WGIVE      (W, 'RIANNON')
C     !END
      call BYE ('RIANNON')
C
      return
      end
