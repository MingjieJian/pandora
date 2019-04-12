      subroutine PACO
     $(NE,K,XKK,AKK,GKK,EMUXK,EXT,V,XLB,WN,GP,P)
C
C     Rudolf Loeser, 1980 Jun 16
C---- Computes and adds the K'th increment to P[Lambda-1], for DURIAN.
C
C     (Note: NZ equals N; NE (i.e. "n to eta") is the length of a
C     reduced set, not greater than N.)
C     !DASH
      save
C     !DASH
      real*8 AKK, AX, EMUXK, EXJ, EXT, GKK, GP, ONE, P, V, VAT, WN, XKK,
     $       XLB
      integer I, J, K, NE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ZERO1, DIVIDE, HI, BYE
C
C               EMUXK(NZ), XLB(NZ), P(NE,NE), WN(NE,NE), GP(NE,NE),
      dimension EMUXK(*),  XLB(*),  P(NE,*),  WN(NE,*),  GP(NE,*),
C
C               EXT(NE), V(NZ)
     $          EXT(*),  V(*)
C
      call HI ('PACO')
C     !BEG
      if(K.eq.1) then
        call ZERO1    (P, (NE*NE))
      end if
C
      AX = AKK/XKK
      do 100 I = 1,NE
        EXT(I) = AX*EMUXK(I)
  100 continue
C
      do 102 J = 1,NE
        EXJ = EXT(J)
        do 101 I = 1,NE
          call DIVIDE (V(I), V(J), VAT)
          GP(I,J) = GKK*VAT*(ONE-XLB(J))
          P(I,J)  = P(I,J)+(EXJ*GP(I,J))*WN(I,J)
  101   continue
  102 continue
C     !END
      call BYE ('PACO')
C
      return
      end
