      subroutine CHAMOIS
     $(WVL,DDL,FDDL,WDL,LDL,DL,K,DP,DW,V,N,PHI,DV,A,U,MUSE,PHC)
C
C     Rudolf Loeser, 1985 Jun 19
C---- Computes (composite) Voigt absorption profile PHI, and
C     also values of DV, A, U and PHC (for each component).
C     !DASH
      save
C     !DASH
      real*8 A, DDL, DL, DP, DV, DW, FDDL, PHC, PHI, U, V, WDL, WVL
      integer K, L, LDL, MUSE, N
C     !DASH
      external ANGORA, KARAKUL, MOVE1, HI, BYE
C
C               DDL(LDL), WDL(N,LDL), DL(K), DP(N,LDL), DW(N), FDDL(N),
      dimension DDL(*),   WDL(*),     DL(*), DP(N,*),   DW(*), FDDL(*),
C
C               PHI(N,K), DV(N,LDL), A(N,LDL), U(N,K,LDL), PHC(N,K,LDL),
     $          PHI(*),   DV(N,*),   A(N,*),   U(N,K,*),   PHC(N,K,*),
C
C               V(N)
     $          V(*)
C
      call HI ('CHAMOIS')
C     !BEG
C---- Loop over all components
      do 100 L = 1,LDL
C       Compute PHC (profile for this component)
        call KARAKUL (WVL, DDL(L), FDDL, DL, K, DP(1,L), DW, V, N,
     $                MUSE, DV(1,L), A(1,L), U(1,1,L), PHC(1,1,L))
  100 continue
      if(LDL.le.1) then
        call MOVE1   (PHC(1,1,1), (N*K), PHI)
      else
C----   Compute PHI (total composite profile)
        call ANGORA  (N, K, LDL, PHC, WDL, PHI)
      end if
C     !END
      call BYE ('CHAMOIS')
C
      return
      end
