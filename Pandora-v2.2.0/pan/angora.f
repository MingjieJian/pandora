      subroutine ANGORA
     $(N,K,LDL,PHC,WDL,PHI)
C
C     Rudolf Loeser, 1992 Apr 22
C---- Computes a composite line profile
C     as the weighted sum of its components.
C     (This is version 2 of ANGORA.)
C     !DASH
      save
C     !DASH
      real*8 PHC, PHI, WDL
      integer I, J, K, LDL, N, NK
C     !DASH
      external SUMPROD, HI, BYE
C
C               PHC(N,K,LDL), WDL(N,LDL), PHI(N,K)
      dimension PHC(N,K,*),   WDL(N,*),   PHI(N,*)
C
      call HI ('ANGORA')
C     !BEG
      NK = N*K
      do 101 J = 1,K
        do 100 I = 1,N
          call SUMPROD (PHI(I,J), PHC(I,J,1), NK, WDL(I,1), N, LDL)
  100   continue
  101 continue
C     !END
      call BYE ('ANGORA')
C
      return
      end
