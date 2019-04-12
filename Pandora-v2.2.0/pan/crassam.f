      subroutine CRASSAM
     $(N,YH,TE,HND,XNE,VT,PMG,DGM,F,G,PTO,PEL,PGS,PTU,PEX,PNH,HELABD,
     $ H2N,VM,GDN,EMN)
C
C     Rudolf Loeser, 2003 Nov 04
C---- Computes approximate initial values
C     of the HSE quantities F and G,
C     where F (aka GDENOM) is the denominator of the expression for G.
C     !DASH
      save
C     !DASH
      real*8 DGM, EMN, F, G, GDN, H2N, HELABD, HND, PEL, PEX, PGS, PMG,
     $       PNH, PTO, PTU, TE, VM, VT, XNE, YH
      integer N
C     !DASH
      external SET1, ZERO1, GHASTLY, ARRDIV, GISELA, HI, BYE
C
C               HELABD(N), PTO(N), PEL(N), PGS(N), PEX(N), TE(N), F(N),
      dimension HELABD(*), PTO(*), PEL(*), PGS(*), PEX(*), TE(*), F(*),
C
C               H2N(N), PNH(N), XNE(N), DGM(N), GDN(N), EMN(N), HND(N),
     $          H2N(*), PNH(*), XNE(*), DGM(*), GDN(*), EMN(*), HND(*),
C
C               G(N), VT(N), VM(N), PTU(N), PMG(N)
     $          G(*), VT(*), VM(*), PTU(*), PMG(*)
C
      call HI ('CRASSAM')
C     !BEG
      call SET1    (HELABD, N, YH)
      call ZERO1   (H2N,    N)
      call ZERO1   (VM,     N)
C
      call GHASTLY (N, PEL, PGS, PTU, PEX, PMG, PNH, PTO, XNE, TE, HND,
     $              HELABD, H2N, VT, VM, GDN, EMN)
      call ARRDIV  (PTO, HND, F, N)
C
      call GISELA  (N, HELABD, DGM, F, G)
C     !END
      call BYE ('CRASSAM')
C
      return
      end
