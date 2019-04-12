      subroutine HYSSOP
     $(N,NL,BDI,BDIJ,XND,XNK,ESG,VEC)
C
C     Rudolf Loeser, 2001 Dec 12
C---- Computes b1, and then the higher b's directly from b ratios.
C     (This is version 2 of HYSSOP.)
C     !DASH
      save
C     !DASH
      real*8 BDI, BDIJ, ESG, VEC, XND, XNK
      integer J, N, NL
C     !DASH
      external ARRMUL, ARRDIV, HI, BYE
C
C               BDI(N,NL), BDIJ(N,NL), XND(N,NL), ESG(N), XNK(N),
      dimension BDI(N,*),  BDIJ(N,*),  XND(N,*),  ESG(*), XNK(*),
C
C               VEC(N)
     $          VEC(*)
C
      call HI ('HYSSOP')
C     !BEG
      call ARRMUL   (XNK,ESG,VEC,N)
      call ARRDIV   (XND(1,1),VEC,BDI(1,1),N)
      do 100 J = 2,NL
        call ARRMUL (BDI(1,1),BDIJ(1,J),BDI(1,J),N)
  100 continue
C     !END
      call BYE ('HYSSOP')
C
      return
      end
