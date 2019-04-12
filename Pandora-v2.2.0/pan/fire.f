      subroutine FIRE
     $(X,W,IW,LL,N,DLLL,XILL,YUL,XNE,DPUL,DWUL,DDLUL,FDDLUL,CDLUL,
     $ COPUL,GTNUL,Z,TNU,PHI,WN,WH,ISR,MIK,IMG,ILFLX,MPROM,DMP1)
C
C     Rudolf Loeser, 1981 Nov 04
C---- Computes weight matrices for a particular line frequency.
C     (This is version 5 of FIRE.)
C     !DASH
      save
C     !DASH
      real*8 CDLUL, COPUL, DDLUL, DLLL, DPUL, DWUL, FDDLUL, GTNUL, PHI,
     $       THREE, TNU, W, WH, WN, X, XILL, XNE, YUL, Z
      integer IL, ILFLX, IMG, IQFIN, IQSFS, ISR, IU, IW, JJTS, JJXM,
     $        LDL, LL, MIK, MPROM, N
      logical DMP1, FIN, GDIL, TAURED
      character TITLE*100
C     !COM
C---- MANAGER     as of 2006 Dec 27
      integer     IBSCR,IZOQ
      dimension   IZOQ(269)
      common      /MANAGER/ IBSCR,IZOQ
C     REAL*8 General Data Block components index.
      equivalence (IZOQ( 22),JJTS )
      equivalence (IZOQ( 95),JJXM )
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
C     !EJECT
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 4),THREE )
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
      equivalence (IQQ( 31),IQSFS)
      equivalence (IQQ( 49),IQFIN)
C     !DASH
      external  SARONG, LAMBDA, AHURA, ZERO1, HI, BYE
C
      dimension X(*), W(*), IW(*)
C
C               DPUL(N,LDL), TNU(N), DDLUL(LDL), CDLUL(LDL), FDDLUL(N),
      dimension DPUL(*),     TNU(*), DDLUL(*),   CDLUL(*),   FDDLUL(*),
C
C               DLLL(1), GTNUL(N), DWUL(N), WH(N,N), WN(N,N), COPUL(N),
     $          DLLL(1), GTNUL(*), DWUL(*), WH(*),   WN(*),   COPUL(*),
C
C               Z(N), XNE(N), PHI(N), IMG(N)
     $          Z(*), XNE(*), PHI(*), IMG(*)
C
      data TAURED /.true./
      data GDIL   /.true./
C     !EJECT
C
      call HI ('FIRE')
C     !BEG
      write (TITLE,100) IU,IL,LL,XILL,DLLL
  100 format(' ','Weight matrix for transition ',I3,'/',I3,
     $           ', Frequency ',I5,',',1PE13.5,
     $           ', Delta-Lambda ',E13.5)
C
      if(IQSFS.le.0) then
C
C----   Plane-parallel atmosphere
C
        if(YUL.eq.-THREE) then
C----     "GENERAL" method - explicit (numeric) angle integration
          call SARONG  (X, W, IW, LL, N, DLLL, XILL, MPROM, XNE, DPUL,
     $                  DWUL, DDLUL, FDDLUL, CDLUL, PHI, COPUL, GTNUL,
     $                  Z, YUL, WN, WH, ILFLX, ISR, MIK, IMG, TITLE,
     $                  DMP1)
        else
C----     Other methods - implicit (analytic) angle integration
          FIN = IQFIN.gt.0
          call LAMBDA  (X, W, IW, TNU, N, N, YUL, FIN, TAURED, GDIL,
     $                  ISR, TITLE, WN)
          if(ILFLX.gt.0) then
            call ZERO1 (WH, (N**2))
          end if
          MIK = 0
        end if
C
      else
C
C----   Spherical atmosphere
C
        call AHURA     (X, W, IW, YUL, WN, WH, ILFLX, ISR, MIK, DLLL,
     $                  MPROM, XNE, DPUL, DWUL, DDLUL, FDDLUL, CDLUL,
     $                  LDL, COPUL, GTNUL, IMG, TITLE, LL)
C
      end if
C     !END
      call BYE ('FIRE')
C
      return
      end
