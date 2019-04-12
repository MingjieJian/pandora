      subroutine VICUNA
     $(WVL,DDL,FDDL,WDL,LDL,DL,K,DP,DW,V,XNE,N,PHI,DV,A,U,MUSE,PHC,
     $ LU,W,IW)
C
C     Rudolf Loeser, 1991 Dec 12
C---- Computes a line absorption profile for Hydrogen.
C     If LU > 0, then a printout of computational details
C     will be written to unit LU.
C     !DASH
      save
C     !DASH
      real*8 A, DDL, DL, DP, DV, DW, FDDL, PHC, PHI, U, V, W, WDL, WVL,
     $       XNE
      integer IL, IN, IPD, IPNT, IS, ISV, IU, IVEC, IW, IXS, JN, K, L,
     $        LDL, LU, MOX, MUSE, MUX, N
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
C     !DASH
C     !EJECT
      external ALPACA, FASAN, ANGORA, MOSS, WGIVE, IGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               DDL(LDL), WDL(N,LDL), PHI(N,K), DP(N,LDL), DW(N), V(N),
      dimension DDL(*),   WDL(*),     PHI(*),   DP(N,*),   DW(*), V(*),
C
C               PHC(N,K,LDL), DV(N,LDL), A(N,LDL), U(N,K,LDL), FDDL(N),
     $          PHC(N,K,*),   DV(N,*),   A(N,*),   U(N,K,*),   FDDL(*),
C
C               XNE(N), DL(K)
     $          XNE(*), DL(*)
C
      dimension IN(3)
      equivalence
     $(IN( 1),IPD   ),(IN( 2),ISV   ),(IN( 3),IVEC  )
C
      dimension JN(1)
      equivalence
     $(JN( 1),IPNT  )
C
      call HI ('VICUNA')
C     !BEG
C     (Get, and allocate, W & IW allotments)
      call ALPACA  (IN, IS,  MOX, 'VICUNA')
      call MOSS    (JN, IXS, MUX, 'VICUNA')
C
C---- Loop over all component lines
      do 100 L = 1,LDL
C       Compute PHC (the profile for this component)
        call FASAN (IU, IL, WVL, DDL(L), FDDL, DL, K, DP(1,L), DW, V,
     $              XNE, N, DV(1,L), A(1,L), U(1,1,L), MUSE,
     $              PHC(1,1,L), LU, W(IPD), W(ISV), W(IVEC), IW(IPNT))
  100 continue
C---- Compute PHI (total composite profile)
      call ANGORA  (N, K, LDL, PHC, WDL, PHI)
C
C     (Give back W & IW allotments)
      call WGIVE   (W,  'VICUNA')
      call IGIVE   (IW, 'VICUNA')
C     !END
      call BYE ('VICUNA')
C
      return
      end
