      subroutine NUMBAT
     $(N,K,XJNU,XKPC,BC,SIG,SC,XKC)
C
C     Rudolf Loeser, 1986 Jul 10
C---- Computes KC and SC: continuum terms for PRD in Emergent Profile.
C     (This is version 2 of NUMBAT.)
C     !DASH
      save
C     !DASH
      real*8 BC, SC, SIG, XJNU, XKC, XKPC
      integer I, J, K, N
C     !DASH
      external ARRADD, ARRDIV, ARRMUL, HI, BYE
C
C               XJNU(N,K), XKPC(N,K), BC(N,K), SIG(N,K), XKC(N,K),
      dimension XJNU(N,*), XKPC(N,*), BC(N,*), SIG(N,*), XKC(N,*),
C
C               SC(N,K)
     $          SC(N,*)
C
      call HI ('NUMBAT')
C     !BEG
      do 100 J = 1,K
        call ARRMUL (XKPC(1,J), BC(1,J),   XKC(1,J), N)
        call ARRMUL (SIG(1,J),  XJNU(1,J), SC(1,J),  N)
        call ARRADD (XKC(1,J),  SC(1,J),   SC(1,J),  N)
        call ARRADD (XKPC(1,J), SIG(1,J),  XKC(1,J), N)
        call ARRDIV (SC(1,J),   XKC(1,J),  SC(1,J),  N)
  100 continue
C     !END
      call BYE ('NUMBAT')
C
      return
      end
