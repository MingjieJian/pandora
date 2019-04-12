      subroutine BOGDAN
     $(X,W,IW,WVLUL,DPUL,DWUL,XNE,DDLUL,FDDLUL,CDLUL,COPUL,GTNUL,VXS,
     $ DL,LL,TNUN,KODE,MPROM,IMG)
C
C     Rudolf Loeser, 1983 Mar 03.
C---- Computes TNU, along the normal direction, for Line Source Function
C     calculation.
C
C     Returns with KODE=1 if TNUN is OK, =0 if not.
C     (This is version 2 of BOGDAN.)
C     !DASH
      save
C     !DASH
      real*8 CDLUL, COPUL, DDLUL, DL, DPUL, DWUL, FDDLUL, GTNUL, TNUN,
     $       VXS, W, WVLUL, X, XNE
      integer IL, IMG, IN, IPHIN, IS, IU, IW, K1, KODE, LDL, LL, MOX,
     $        MPROM, N
      character LABEL*100
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (LINKDS(12),LDL  )
C     !DASH
C     !EJECT
      external  ALFRED, YMUIR, MIKE, WGIVE, HI, BYE
      intrinsic min, max
C
      dimension X(*), W(*), IW(*)
C
C               CDLUL(LDL), DPUL(N,LDL), TNUN(N), DDLUL(LDL), COPUL(N),
      dimension CDLUL(*),   DPUL(*),     TNUN(*), DDLUL(*),   COPUL(*),
C
C               GTNUL(N), IMG(N), VXS(N), XNE(N), DWUL(N), FDDLUL(N),
     $          GTNUL(*), IMG(*), VXS(*), XNE(*), DWUL(*), FDDLUL(*),
C
C               DL(1)
     $          DL(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IPHIN )
C
      data K1 /1/
C
      call HI ('BOGDAN')
C     !BEG
C     (Get, and allocate, W allotment)
      call ALFRED (IN, IS, MOX, 'BOGDAN')
C
C---- Get Absorption Profile (Note: Mu=1)
      call YMUIR  (W, IW, WVLUL, DL, K1, DPUL, DWUL, XNE, VXS, N,
     $             DDLUL, FDDLUL, CDLUL, LDL, MPROM, W(IPHIN))
C
C---- Get Monochromatic Optical Depth
      write (LABEL,100) IU,IL,LL,DL(1)
  100 format(' ','Line (',I2,',',I2,'), Monochromatic TNU for',I3,
     $           '. Frequency; DL =',F15.7)
      call MIKE   (X, W, N, 1, N, W(IPHIN), COPUL, GTNUL, LABEL, TNUN,
     $             KODE, IMG)
      KODE = 1-max(min(KODE,1),0)
C
C     (Give back W allotment)
      call WGIVE  (W, 'BOGDAN')
C     !END
      call BYE ('BOGDAN')
C
      return
      end
