      subroutine BARAMA
     $(LU,DL,K,N,Z,DW,DP,VEX,XNE,MPROM,LDL,DDL,FDDL,CDL,W,IW)
C
C     Rudolf Loeser, 1982 Dec 28
C---- Produces a "profile Analysis".
C     (This is version 2 of BARAMA.)
C     !DASH
      save
C     !DASH
      real*8 CDL, DDL, DL, DP, DW, FDDL, VEX, W, XNE, Z
      integer IHSSP, IL, IU, IW, K, LDL, LU, MPROM, N
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
      equivalence (KZQ(136),IHSSP)
C
C---- LINUS       as of 2004 May 12
      integer     LINKDS
      dimension   LINKDS(22)
      common      /LINUS/ LINKDS
C     Line source function calculation control parameters for the
C     current transition as set up by "PET" (and printed by "LINSEED").
C     IU    - index of upper level
C     IL    - index of lower level
C     KLIN  - line "type" code (1: radiative, 2: passive, etc)
C     ICE   - PRD calculation control
C     IPRO  - emergent profiles calculation control
C     METSE - statistical equilibrium calculation method selector
C     METSF - LSF calculation method selector (QR, RT, GR)
C     IBRSW - damping components selector
C     INKSW - input opacity signal
C     LSFT  - LSF solution code (0: full, 1:direct, etc)
C     ILFLX - line flux calculation control
C     LDL   - number of line components
C     LINT  - frequency integration range (half vs. full profile)
C     LSFP  - LSF printout control
C     IFDB  - LSF background control (constant vs. varying)
C     ISBG  - blended line profile plot mode switch
C     KBT   - length of input table XIBLUT
C     KRT   - length of input table XIREDT
C     KST   - length of input table XISYMT
C     KTRN  - length of actual tables XI and DL
C     LOML  - "line-background-continuum-opacity" control
C     ....  - (available)
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
C     !DASH
C     !EJECT
      external ABJECT, LAPWING, LINER, PEEK, HI, BYE
C
      dimension W(*), IW(*)
C
C               DL(K), DW(N,LDLMX), DP(N), VEX(N), DDL(LDLMX), FDDL(N),
      dimension DL(*), DW(*),       DP(*), VEX(*), DDL(*),     FDDL(*),
C
C               CDL(LDLMX), Z(N), XNE(N)
     $          CDL(*),     Z(*), XNE(*)
C
      call HI ('BARAMA')
C     !BEG
      if((LU.gt.0).and.(NVY.gt.1)) then
        call ABJECT  (LU)
        call LAPWING (LU, IU, IL, LDL, (NVY-1))
C
        call LINER   (1, LU)
        if(SPHERE) then
          write (LU,100)
  100     format(' ','Along the ray normal to the center of the disk')
        else
          write (LU,101) EMOO
  101     format(' ','Mu =',F7.4)
        end if
C
        call PEEK    (K, DL, N, Z, DW, DP, VEX, MPROM, XNE, WVLTRN,
     $                DDL, FDDL, IHSSP, CDL, LDL, IU, IL, LU, W, IW)
      end if
C     !END
      call BYE ('BARAMA')
C
      return
      end
