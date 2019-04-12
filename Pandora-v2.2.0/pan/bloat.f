      subroutine BLOAT
     $(K,L,LF,FLX,WVL,DL,WVNUM,WTAB,EMU,EMUF,WMUF,YHZFB,TCINT,TCFLX,
     $ PROGLI,LDL,DDL,PGD,NO,LUG,IJECT,W,IW)
C
C     Rudolf Loeser, 2005 Jan 06
C---- Displays flow-broadened profiles.
C     !DASH
      save
C     !DASH
      real*8 DDL, DL, EMU, EMUF, PGD, PROGLI, TCFLX, TCINT, W, WMUF,
     $       WTAB, WVL, WVNUM, YHZFB, dummy
      integer IFHZ, IJECT, IKODE, IMUX, IMYX, IN, IRES, IS, ISB1, ITC,
     $        IW, IWINT, IWS, IYY, JN, K, KLIN, KRES, KTF, KTI, L, LDL,
     $        LF, LUG, MOX, MUX, NO, jummy
      logical FLX, INCRAD
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
      equivalence (LINKDS( 3),KLIN )
C     !DASH
      external BLIMP, BIGGY, ZERO1, ZEROI, DOROTHY, MARZI, ZOE, ORNATI,
     $         ZAMIDAR, ROT, OOZE, ZENOBIA, APRICOT, WGIVE, IGIVE,
     $         HI, BYE
C
      dimension W(*), IW(*)
C
C               WMUF(LF), WTAB(KM), YHZFB(KM,LF), TCFLX(KM), WVNUM(KM),
      dimension WMUF(*),  WTAB(*),  YHZFB(K ,*),  TCFLX(*),  WVNUM(*),
C
C               TCINT(KM,L), PGD(3,LDLMX), DDL(LDL), EMU(L), EMUF(LF),
     $          TCINT(K ,*), PGD(*),       DDL(*),   EMU(*), EMUF(*),
C
C               DL(KM)
     $          DL(*)
C
      dimension IN(5)
      equivalence
     $(IN( 1),ITC   ),(IN( 2),IWINT ),(IN( 3),IRES  ),(IN( 4),IYY   ),
     $(IN( 5),IFHZ  )
C
      dimension JN(3)
      equivalence
     $(JN( 1),IKODE ),(JN( 2),IMUX  ),(JN( 3),IMYX  )
C     !EJECT
C
      call HI ('BLOAT')
C     !BEG
      if(NO.gt.0) then
C----   (Get, and allocate, W & IW allotments)
        call BLIMP        (IN, IS,  MOX, 'BLOAT')
        call BIGGY        (JN, IWS, MUX, 'BLOAT')
C
C----   Initialize
        call ZERO1        (W(IYY),       K)
        call ZEROI        (IW(IKODE), 1, K)
        call ZEROI        (IW(IMUX),  1, K)
        call ZEROI        (IW(IMYX),  1, K)
        INCRAD = .false.
        SPHERE = .false.
        EXPAND = .false.
        LFBV = 1
        LFB  = 1
        ISB1 = 0
        NVY  = 0
C----   Loop over all look-angles
        do 100 MF = 1,LF
          call DOROTHY    (EMUF, EMU, L)
          if(FLX) then
C----       Accumulate flux
            call MARZI    (MF, K, YHZFB(1,MF), WMUF, W(IWINT))
          end if
C----     Get continuum
          call ZOE        (INCRAD, L, K, TCINT, dummy, W(ITC), KTI,
     $                     dummy, jummy)
          if(MUK.gt.0) then
C----       Print line profile for this look-angle
            call ZAMIDAR  (1, 0, 0, K, DL, WVNUM, WTAB, WVL,
     $                     YHZFB(1,MF), W(IYY), IW(IKODE), IW(IMUX),
     $                     IW(IMYX), W(ITC), KTI, W(IRES), KRES, KLIN,
     $                     ISB1, .false., NO, LDL, DDL, IJECT, W)
            if(KRES.gt.0) then
C----         Plot residual profile
              call ORNATI (W, K, LDL, WVL, DL, WVNUM, WTAB, PROGLI,
     $                     DDL, PGD, W(IRES), KRES, dummy, 0,
     $                     dummy, 0, LUG)
            end if
          end if
  100   continue
C----   Plot absolute intensity
        call ROT          (LUG, K, L, WTAB, YHZFB, INCRAD, dummy,
     $                     PROGLI)
C     !EJECT
        if(FLX) then
C----     Get continuum
          call OOZE    (K, TCFLX, W(ITC), KTF)
C----     Print line flux profile
          call ZENOBIA (NO, K, LF, EMUF, DL, WVL, WVNUM, WTAB,
     $                  W(IWINT), dummy, dummy, W(IFHZ), 1, KLIN,
     $                  W(ITC), KTF, .false., ISB1, LDL, DDL, IJECT,
     $                  W)
C----     Plot
          call APRICOT (LUG, K, WVL, DL, WVNUM, WTAB, W(IFHZ), PROGLI,
     $                  LDL, DDL, PGD, W)
        end if
C
C       (Give back W & IW allotments)
        call WGIVE     (W,  'BLOAT')
        call IGIVE     (IW, 'BLOAT')
      end if
C     !END
      call BYE ('BLOAT')
C
      return
      end
