      subroutine EPIC
     $(N,DL,VX,XNE,DP,DW,DDL,FDDL,CDL,PHI,PHIW,W,IW,LL,MPROM,DMP1)
C
C     Rudolf Loeser, 1977 Jun 21
C---- Compute PHIs for Diana Data Blocks.
C     !DASH
      save
C     !DASH
      real*8 CDL, CORE, DDL, DL, DP, DW, FDDL, PHI, PHIW, VX, W, XNE
      integer I, IW, K1, KRP, LDL, LL, MPROM, N
      logical DMP1
C     !COM
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
      equivalence (LINKDS(12),LDL  )
C     !DASH
      external YMUIR, MOVED, YULE, HI, BYE
C
      dimension W(*), IW(*)
C
C               PHI(N), DDL(LDL), VX(N), DP(N,LDL), PHIW(N,N), FDDL(N),
      dimension PHI(*), DDL(*),   VX(*), DP(*),     PHIW(N,*), FDDL(*),
C
C               CDL(LDL), XNE(N), DW(N), DL(1)
     $          CDL(*),   XNE(*), DW(*), DL(*)
 
      data CORE,K1 /0.D0, 1/
C     !EJECT
C
      call HI ('EPIC')
C     !BEG
C---- (Note: VX=0)
      call YMUIR   (W, IW, CORE, DL, K1, DP, DW, XNE, VX, N, DDL,
     $              FDDL, CDL, LDL, MPROM, PHI)
C
C---- (Note: VX=0 means that all rows of PHIW = PHI!)
      do 100 I = 1,N
        call MOVED (PHI, 1, N, PHIW(I,1), N, N)
  100 continue
C
      if(LL.eq.1) then
        KRP = 1
      end if
      if(DMP1) then
        call YULE  (LL, KRP, N, DP, DW, VX, PHI)
      end if
C     !END
      call BYE ('EPIC')
C
      return
      end
