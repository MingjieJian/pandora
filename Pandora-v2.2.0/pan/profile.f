      subroutine PROFILE
     $(WVL,DDL,FDDL,CDL,LDL,DL,K,MPROM,MUSE,XNE,DP,DW,V,N,PHI,
     $ DV,A,U,PHC,W,IW)
C
C     Rudolf Loeser, 1985 Jun 19
C---- Computes the composite line absorption profiles PHI,
C     and the LDL component absorption profiles PHC.
C     Also returns the LDL components of the intermediate terms
C     DV, A and U.
C
C---- Given:
C
C     MPROM - calculation mode switch:
C             = 0 means: Voigt profile only,
C             = 1 means: convolve with Stark profile (Hydrogen),
C     MUSE  - calculation mode switch:
C             = 0 means: set U = 0 (special case),
C             = 1 means: compute U normally,
C     DDL   - blended line components offsets (Angstroms),
C     FDDL  - depth-dependent multiplier for DDL
C     CDL   - blended line components weights,
C     DL    - profile sampling points (delta Lambda) (Angstroms),
C     XNE   - electron density (used when MPROM = 1),
C     DP    - Damping parameter,
C     DW    - Doppler width,
C     V     - expansion velocity (projected into the current direction),
C     WVL   - reference/core wavelength (Angstroms) (not used when V=0).
C
C     (This is version 4 of PROFILE.)
C     !DASH
      save
C     !DASH
      real*8 A, CDL, DDL, DL, DP, DV, DW, FDDL, PHC, PHI, U, V, W, WVL,
     $       XNE
      integer IN, IS, IW, IWDL, K, LDL, LU, MOX, MPROM, MUSE, N
C     !DASH
C     !EJECT
      external ITONOS, AARON, VICUNA, CHAMOIS, WGIVE, HI, BYE
C
      dimension W(*), IW(*)
C
C               DDL(LDL), CDL(LDL), DL(K), DP(N,LDL), A(N,LDL), XNE(N),
      dimension DDL(*),   CDL(*),   DL(*), DP(*),     A(*),     XNE(*),
C
C               PHI(N,K), DV(N,LDL), U(N,K,LDL), PHC(N,K,LDL), FDDL(N),
     $          PHI(*),   DV(*),     U(*),       PHC(*),       FDDL(*),
C
C               DW(N), V(N)
     $          DW(*), V(*)
C
      dimension IN(1)
      equivalence
     $(IN( 1),IWDL  )
C
      data LU /0/
C
      call HI ('PROFILE')
C     !BEG
C     (Get W allotment)
      call ITONOS    (IN, IS, MOX, 'PROFILE')
C
C---- Set up "effective" blended line components weights
      call AARON     (N, LDL, CDL, DDL, FDDL, DW, W(IWDL))
C
      if(MPROM.le.0) then
C----   If this is a single line (LDL=1), then PHC will be set equal
C       to PHI; if this is a blended line (LDL>1), then PHI will be
C       the final composite profile, and PHC will contain the individual
C       component profiles.
C
        call CHAMOIS (WVL, DDL, FDDL, W(IWDL), LDL, DL, K, DP, DW, V,
     $                N, PHI, DV, A, U, MUSE, PHC)
      else
C----   Hydrogen when Stark broadening has not been included in the
C       Voigt profile; instead, separate Voigt and Stark profiles must
C       be computed and convolved. (Blended lines as above for CHAMOIS)
C
        call VICUNA  (WVL, DDL, FDDL, W(IWDL), LDL, DL, K, DP, DW, V,
     $                XNE, N, PHI, DV, A, U, MUSE, PHC, LU, W, IW)
      end if
C
C     (Give back W allotment)
      call WGIVE     (W, 'PROFILE')
C     !END
      call BYE ('PROFILE')
C
      return
      end
