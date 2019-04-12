      subroutine PTAH
     $(N,HN1,XNE,TE,GP,DEL,EPS,G,RM,RD,BDHM,XNHM)
C
C     Rudolf Loeser, 1971 Jun 17
C---- Computes BDHM, etc, for OSIRIS.
C     !DASH
      save
C     !DASH
      real*8 BDHM, C1, C2, C3, C4, D1, DEL, EPS, EXPTH, G, GP, GPOD,
     $       HN1, ONE, RD, RM, RTH3, RTHETA, TE, THETA, XH, XNE, XNHM
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, HI, BYE
C
C               HN1(N), XNHM(N), TE(N), GP(N), DEL(N), EPS(N), BDHM(N),
      dimension HN1(*), XNHM(*), TE(*), GP(*), DEL(*), EPS(*), BDHM(*),
C
C               RD(N), XNE(N), RM(N), G(N)
     $          RD(*), XNE(*), RM(*), G(*)
C
      data C1,C2,C3,C4 /2.D-9, 3.757D1, 1.736D0, 5.04D3/
      data D1          /2.89D-22/
C
      call HI ('PTAH')
C     !BEG
      do 100 I = 1,N
        call DIVIDE (C4, TE(I), THETA)
        RTHETA = sqrt(THETA)
        RTH3   = RTHETA**3
        GP(I)  = C1*HN1(I)
        EXPTH  = exp(C3*THETA)
        call DIVIDE (XNE(I), HN1(I), XH)
        DEL(I) = C2*RTHETA*EXPTH*XH
        call DIVIDE ((C1*XNE(I)), RTH3, EPS(I))
        call DIVIDE (GP(I), (ONE+DEL(I)), GPOD)
        G(I) = GPOD+EPS(I)
C
        call DIVIDE ((RD(I)+G(I)), (RM(I)+G(I)), BDHM(I))
        XNHM(I) = D1*HN1(I)*XNE(I)*RTH3*EXPTH*BDHM(I)
C
  100 continue
C     !END
      call BYE ('PTAH')
C
      return
      end
