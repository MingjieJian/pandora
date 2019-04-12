      subroutine CARAMAN
     $(X,W,IW,N,DLLL,VXS,MPROM,XNE,DPUL,DWUL,DDLUL,FDDLUL,CDLUL,PHI,
     $ XILL,COPUL,GTNUL,LL,XMU,LG,TAU,TMU,IMG,MIK,DMP1)
C
C     Rudolf Loeser, 1981 May 07
C---- Computes a set of TMU values, for SARONG.
C     !DASH
      save
C     !DASH
      real*8 CDLUL, COPUL, CORE, DDLUL, DLLL, DPUL, DWUL, FDDLUL, GTNUL,
     $       PHI, TAU, TMU, VXS, W, X, XILL, XMU, XNE
      integer IL, IMG, IU, IW, K1, LDL, LG, LL, MIK, MPROM, N
      logical DMP1
      character LABEL*100
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
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
      equivalence (LINKDS(12),LDL  )
C     !DASH
C     !EJECT
      external YMUIR, MIKE, TANG, MOLLIE, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               FDDLUL(N), DPUL(N,LDL), DWUL(N), DDLUL(LDL), TMU(N,LG),
      dimension FDDLUL(*), DPUL(*),     DWUL(*), DDLUL(*),   TMU(*),
C
C               COPUL(N), XNE(N), IMG(N), VXS(N), CDLUL(LDL), GTNUL(N),
     $          COPUL(*), XNE(*), IMG(*), VXS(*), CDLUL(*),   GTNUL(*),
C
C               XMU(LG), PHI(N), DLLL(1), TAU(N)
     $          XMU(*),  PHI(*), DLLL(1), TAU(*)
C
      data CORE,K1 /0.D0, 1/
C
      call HI ('CARAMAN')
C     !BEG
C---- Compute Line Absorption Profile (Note: VXS=0)
      call YMUIR    (W, IW, CORE, DLLL, K1, DPUL, DWUL, XNE, VXS, N,
     $               DDLUL, FDDLUL, CDLUL, LDL, MPROM, PHI)
C
C---- Compute Optical Depth along normal ray
      write (LABEL,100) IU,IL,LL,DLLL
  100 format(' Line',I3,'/',I2,' Monochromatic TMU at',I3,
     $       '. Frequency. DL =',F15.7)
C
      call MIKE     (X, W, N, 1, N, PHI, COPUL, GTNUL, LABEL, TAU,
     $               MIK, IMG)
C
C---- Compute Optical Depths along all directions
      call TANG     (XMU, LG, TAU, N, TMU)
      if(DMP1) then
        call MOLLIE (LL, TMU, N, LG, LABEL, MIK, XMU, TAU)
      end if
C     !END
      call BYE ('CARAMAN')
C
      return
      end
