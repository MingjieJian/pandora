      subroutine DIPRON
     $(W,IW,N,K,LDL,VSW,LTE,WVL,DL,Z,TE,V,VR,VEX,DP,DW,XNE,MPROM,DDL,
     $ FDDL,CDL,COPTRN,BCTRN,GTN,GTNL,STRN,STRNL,TNU,TNUL,SNU,SNUL,
     $ NO,LUA)
C
C     Rudolf Loeser, 2000 Jul 20
C---- Computes monochromatic TAU and S for line profiles calculation,
C     and also computes absorption profile with optional analysis.
C     !DASH
      save
C     !DASH
      real*8 BCTRN, CDL, COPTRN, DDL, DL, DP, DW, FDDL, GTN, GTNL, SNU,
     $       SNUL, STRN, STRNL, TE, TNU, TNUL, V, VEX, VR, W, WVL, XNE,
     $       Z, dummy
      integer ICE, IDWU, IFINT, IGTU, IIMG, IL, IN, IPHI, IS, ISINT, IU,
     $        IW, IWS, IXKPL, IXKPNU, JN, K, LDL, LUA, MOX, MPROM, MUX,
     $        N, NO
      logical LTE, VSW
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
      equivalence (LINKDS( 4),ICE  )
C     !DASH
C     !EJECT
      external RIANNON, DESPINA, IRONDA, IMMAKE, WGIVE, IGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               DP(N,LDLMX), COPTRN(N,KM), BCTRN(N,KM), GTNL(N), TE(N),
      dimension DP(*),       COPTRN(*),    BCTRN(*),    GTNL(*), TE(*),
C
C               STRN(N,KM), STRNL(N,KM), DDL(LDLMX), CDL(LDLMX), VR(N),
     $          STRN(*),    STRNL(*),    DDL(*),     CDL(*),     VR(*),
C
C               TNU(N,KM), TNUL(N,KM), SNU(N,KM), FDDL(N), DW(N), Z(N),
     $          TNU(*),    TNUL(*),    SNU(*),    FDDL(*), DW(*), Z(*),
C
C               SNUL(N,KM), GTN(N), DL(KM), VEX(N), XNE(N), V(N)
     $          SNUL(*),    GTN(*), DL(*),  VEX(*), XNE(*), V(*)
C
      dimension IN(7)
      equivalence
     $(IN( 1),IPHI  ),(IN( 2),IDWU  ),(IN( 3),IXKPNU),(IN( 4),ISINT ),
     $(IN( 5),IFINT ),(IN( 6),IGTU  ),(IN( 7),IXKPL )
C
      dimension JN(1)
      equivalence
     $(JN( 1),IIMG  )
C
      call HI ('DIPRON')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call IRONDA    (IN, IS,  MOX, 'DIPRON')
      call IMMAKE    (JN, IWS, MUX, 'DIPRON')
C
C---- Compute absorption profile
C     (and print optional dump, and profile analysis)
      call RIANNON   (VSW, DL, K, N, Z, TE, V, VR, VEX, DP, DW, XNE,
     $                MPROM, W(IDWU), W(IPHI), LDL, DDL, FDDL, CDL, W,
     $                IW, LUA)
C---- Compute TAU and S, non-LTE
      call DESPINA   (1, Z, GTN, W(IPHI), COPTRN, W(IXKPNU), TNU,
     $                SNU, STRN, BCTRN, DL, K, N, IU, IL, NO, ICE,
     $                W, WVL, VEX, W(ISINT), W(IFINT), W(IGTU), DW,
     $                W(IDWU), W(IXKPL), IW(IIMG))
      if(LTE) then
C----   Compute TAU and S, LTE
        call DESPINA (0, Z, GTNL, W(IPHI), COPTRN, W(IXKPNU), TNUL,
     $                SNUL, STRNL, BCTRN, DL, K, N, IU, IL, NO, 0,
     $                W, dummy, dummy, W(ISINT), W(IFINT), W(IGTU), DW,
     $                W(IDWU), W(IXKPL), IW(IIMG))
      end if
C
C     (Give back W & IW allotments)
      call WGIVE     (W,  'DIPRON')
      call IGIVE     (IW, 'DIPRON')
C     !END
      call BYE ('DIPRON')
C
      return
      end
