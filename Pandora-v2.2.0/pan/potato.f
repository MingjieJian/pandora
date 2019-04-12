      subroutine POTATO
     $(S,SLUL,BTEUL,BTRUL,CSFUL,BCUL,XNU,SET,GTNUL,COPUL,DPUL,DWUL,
     $ MPROM,DDLUL,FDDLUL,CDLUL,PHI,ALFIJ,TE,XNE,BATAIJ,TR,BATRIJ,VXS,
     $ WVL,W,IW)
C
C     Rudolf Loeser, 1975 Dec 04
C---- Computes data for Passive Lines.
C     !DASH
      save
C     !DASH
      real*8 ALFIJ, BATAIJ, BATRIJ, BCUL, BTEUL, BTRUL, CDLUL, COPUL,
     $       CSFUL, DDLUL, DL, DPUL, DWUL, FDDLUL, GTNUL, PHI, S, SET,
     $       SLUL, TE, TR, VXS, W, WVL, XNE, XNU
      integer IL, IQCSF, IQCSW, IU, IW, K1, LDL, MPROM, N
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
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
      equivalence (IQQ( 46),IQCSF)
      equivalence (IQQ( 14),IQCSW)
C     !EJECT
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
      external YMUIR, PLANK, PUCCOON, MURIEL, JOKE, HI, BYE
C
      dimension W(*), IW(*)
C
C               VXS(N), SLUL(N), BTEUL(N), BTRUL(N), CSFUL(N), BCUL(N),
      dimension VXS(*), SLUL(*), BTEUL(*), BTRUL(*), CSFUL(*), BCUL(*),
C
C               XNU(NSL), SET(N,MUL), GTNUL(N), DWUL(N), BATAIJ(N,MUL),
     $          XNU(*),   SET(*),     GTNUL(*), DWUL(*), BATAIJ(*),
C
C               XNE(N), DPUL(N,LDL), FDDLUL(N), DDLUL(LDL), CDLUL(LDL),
     $          XNE(*), DPUL(*),     FDDLUL(*), DDLUL(*),   CDLUL(*),
C
C               PHI(N), ALFIJ(MUL), BATRIJ(N,MUL), COPUL(N), TR(N,NSL),
     $          PHI(*), ALFIJ(*),   BATRIJ(*),     COPUL(*), TR(*),
C
C               TE(N), S(N)
     $          TE(*), S(*)
C
      dimension DL(1)
C
      data DL(1),K1 /0.D0, 1/
C     !EJECT
C
      call HI ('POTATO')
C     !BEG
C---- Compute absorption profile, PHI (Note: Mu=1)
      call YMUIR   (W, IW, WVL, DL, K1, DPUL, DWUL, XNE, VXS, N, DDLUL,
     $              FDDLUL, CDLUL, LDL, MPROM, PHI)
C---- Compute Planck functions, BTE and BTR
      call PLANK   (IU, IL, N, XNU, ALFIJ, TE, BATAIJ, BTEUL)
      if((IQCSF.le.0).and.(IQCSW.gt.0)) then
        call PLANK (IU, IL, N, XNU, ALFIJ, TR, BATRIJ, BTRUL)
      end if
C---- Compute Continuum Source function, BC
      call PUCCOON (N, CSFUL, BTEUL, BTRUL, BCUL)
C---- Compute Monochromatic Source Function, SL
      call MURIEL  (N, IU, IL, XNU, SET, SLUL)
C---- Compute Line Source function, S
      call JOKE    (N, PHI, GTNUL, SLUL, COPUL, BCUL, S)
C     !END
      call BYE ('POTATO')
C
      return
      end
