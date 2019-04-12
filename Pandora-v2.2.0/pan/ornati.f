      subroutine ORNATI
     $(W,K,LDL,CORE,DL,WVNUM,WTAB,PROGLI,DDL,PGD,RCN,KRCN,RCL,
     $ KRCL,RCI,KRCI,NO)
C
C     Rudolf Loeser, 1991 Jun 20
C---- Drives ORANGE to plot residual intensities.
C     !DASH
      save
C     !DASH
      real*8 CORE, DDL, DL, PGD, PROGLI, RCI, RCL, RCN, SCORE, W, WTAB,
     $       WVNUM
      integer IL, IN, IS, ISBG, ISDL, ISRCI, ISRCL, ISRCN, IU, IWTAB,
     $        IWVNM, J, K, KOUNT, KRCI, KRCL, KRCN, LBLU, LDL, LRED,
     $        MOX, NO
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
      equivalence (LINKDS(16),ISBG )
      equivalence (LINKDS( 1),IU   )
      equivalence (LINKDS( 2),IL   )
C     !DASH
      external ORANGE, NEPEAN, MOVE1, CONSUB, INTORA, WUMBLE, SNUGUD,
     $         WGIVE, HI, BYE
C
      dimension W(*)
C
C               WVNUM(KM), WTAB(KM), RCI(KM), PGD(3,LDLMX), DDL(LDLMX),
      dimension WVNUM(*),  WTAB(*),  RCI(*),  PGD(3,*),     DDL(*),
C
C               DL(KM), RCN(KM), RCL(KM)
     $          DL(*),  RCN(*),  RCL(*)
C
      dimension IN(6)
      equivalence
     $(IN( 1),ISDL  ),(IN( 2),ISRCN ),(IN( 3),ISRCL ),(IN( 4),ISRCI ),
     $(IN( 5),IWTAB ),(IN( 6),IWVNM )
C     !EJECT
C
      call HI ('ORNATI')
C     !BEG
      if((NO.gt.0).and.(MUK.gt.0)) then
C----   Plot for entire range of DL values
        call ORANGE      (NO, IU, IL, WTAB, K, PROGLI, RCN, KRCN,
     $                    RCL, KRCL, RCI, KRCI, 0, LDL)
C
        if((LDL.gt.1).and.(ISBG.gt.0)) then
C         (Get, and allocate, W allotment)
          call INTORA    (IN, IS, MOX, 'ORNATI')
C
C----     Need separate plots of blended line components
          do 100 J = 1,LDL
            call NEPEAN  (DL, K, PGD(2,J), PGD(3,J), LBLU, LRED)
            KOUNT = LRED-LBLU+1
            SCORE = CORE+DDL(J)
            call MOVE1   (DL(LBLU), KOUNT, W(ISDL))
            call CONSUB  (DDL(J), W(ISDL), KOUNT)
            call MOVE1   (RCN(LBLU), KOUNT, W(ISRCN))
            if(KRCL.gt.0) then
              call MOVE1 (RCL(LBLU), KOUNT, W(ISRCL))
            end if
            if(KRCI.gt.0) then
              call MOVE1 (RCI(LBLU), KOUNT, W(ISRCI))
            end if
            call SNUGUD  (KOUNT, W(ISDL), SCORE, W(IWVNM), W(IWTAB), 1)
            call ORANGE  (NO, IU, IL, W(IWTAB), KOUNT, PROGLI,
     $                    W(ISRCN), KRCN, W(ISRCL), KRCL, W(ISRCI),
     $                    KRCI, J, LDL)
  100     continue
C
C         (Give back W allotment)
          call WGIVE     (W, 'ORNATI')
C
C----     Restore data in common block MOSTAR (Which was destroyed
C         by those special calls to SNUGUD, above).
          call WUMBLE    (K, DL, WVNUM, WTAB, CORE, 1)
        end if
C
      end if
C     !END
      call BYE ('ORNATI')
C
      return
      end
