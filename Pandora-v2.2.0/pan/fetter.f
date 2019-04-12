      subroutine FETTER
     $(N,NL,KAMB,Z,HND,HK,RABD,HEND,HE1,BETA,HE2K,PLK,RND,PALBET,
     $ PBETAL,DEE,DELTA,F,G,R,S,XN,SPKL,ADDR,ADDS,HEK,HE21)
C
C     Rudolf Loeser, 1988 Jun 16
C---- Computes f, g, r and s, for Special-N1 (diffusion) calculation.
C     (This is version 2 of FETTER.)
C     !DASH
      save
C     !DASH
      real*8 ADDR, ADDS, BETA, DEE, DELTA, F, G, HE1, HE21, HE2K, HEK,
     $       HEND, HK, HND, PALBET, PBETAL, PLK, R, RABD, RND, S, SPKL,
     $       XN, Z
      integer KAMB, N, NL
C     !DASH
      external  MASHA, KOLAR, ARRADD, ARRMUL, NATAL, MOVE1, HI, BYE
C
C               HK(N), HE2K(N), Z(N), XN(N), RABD(N), HEND(N), BETA(N),
      dimension HK(*), HE2K(*), Z(*), XN(*), RABD(*), HEND(*), BETA(*),
C
C               S(N), HND(N), HE1(N), DELTA(7,N), RND(N,NL), PALBET(N),
     $          S(*), HND(*), HE1(*), DELTA(7,*), RND(*),    PALBET(*),
C
C               SPKL(N), PLK(N,NL), DEE(4,5,N), ADDR(N), ADDS(N), R(N),
     $          SPKL(*), PLK(*),    DEE(*),     ADDR(*), ADDS(*), R(*),
C
C               F(N), G(N), PBETAL(N), HE21(N), HEK(N)
     $          F(*), G(*), PBETAL(*), HE21(*), HEK(*)
C     !EJECT
C
      call HI ('FETTER')
C     !BEG
C---- Compute f and g
      call MASHA    (N,KAMB,HND,HK,RABD,HEND,BETA,HE2K,HE1,DEE,DELTA,
     $               F,G)
C
C---- Compute r
      call KOLAR    (KAMB,N,NL,PLK,SPKL,RND,R,PALBET,PBETAL)
      if(KAMB.eq.3) then
        call MOVE1  (PBETAL,N,ADDR)
        call ARRADD (R,ADDR,R,N)
      end if
      if(KAMB.eq.1) then
        call ARRMUL (R ,HND ,R,N)
      else
        call ARRMUL (R ,HEND,R,N)
      end if
C
C---- Compute s (and XN, the "n" for s)
      call NATAL    (KAMB,N,HND,HE1,HEK,HE21,HE2K,HEND,XN)
      call ARRMUL   (XN,SPKL,S,N)
      if(KAMB.eq.3) then
        call ARRMUL (HE1,PALBET,ADDS,N)
        call ARRADD (S,ADDS,S,N)
      end if
C     !END
      call BYE ('FETTER')
C
      return
      end
