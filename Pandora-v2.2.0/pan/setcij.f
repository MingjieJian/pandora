      subroutine SETCIJ
     $(X,IX,NO,P,TEX,XNE,XNC,XNU,XNUC,TER,CEI,CMCE,CACE,AIJ,AATIJ,
     $ NPQ,LRQ,CIJAD,CIJ,FCJ,CHIJ,FCE,KIJ,SIJ)
C
C     Rudolf Loeser, 1978 Nov 14
C---- Recomputes and prints collisional transition rates
C     (This is version 2 of SETCIJ.)
C     !DASH
      save
C     !DASH
      real*8 AATIJ, AIJ, CACE, CEI, CHIJ, CIJ, CIJAD, CMCE, FCE, FCJ, P,
     $       SIJ, TER, TEX, X, XNC, XNE, XNU, XNUC
      integer I, IX, KIJ, LRQ, N, NL, NO, NPQ
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
C     !DASH
C     !EJECT
      external ZEST, GUEST, NEST, PETCIJ, HI, BYE
C
      dimension X(*), IX(*)
C
C               P(NSL), TEX(N), XNE(N), XNC(N), CEI(NTE,MUL), TER(NTE),
      dimension P(*),   TEX(*), XNE(*), XNC(*), CEI(*),       TER(*),
C
C               CIJ(N,NL**2), CIJAD(N,NL**2), FCJ(N,NL), CHIJ(N,NL**2),
     $          CIJ(*),       CIJAD(*),       FCJ(*),    CHIJ(*),
C
C               XNUC(NSL), CMCE(MUL), CACE(MUL), KIJ(NL,NL), FCE(N,NT),
     $          XNUC(*),   CMCE(*),   CACE(*),   KIJ(*),     FCE(*),
C
C               SIJ(N,NL**2), AIJ(NL,NL), NPQ(NSL), LRQ(NSL), XNU(NSL),
     $          SIJ(*),       AIJ(*),     NPQ(*),   LRQ(*),   XNU(*),
C
C               AATIJ(NL,NL)
     $          AATIJ(*)
C
      call HI ('SETCIJ')
C     !BEG
C---- Calculate SIJ, collisions with electrons
      do 100 I = 1,N
        call ZEST (I, N, NL, P, TEX(I), XNC(I), XNE(I), XNU, XNUC,
     $             TER, CEI, AIJ, AATIJ, NPQ, LRQ, CMCE, CACE, SIJ)
  100 continue
C
C---- Calculate CIJ using FCE (enhancement factor)
      call GUEST  (N, NL, FCE, KIJ, SIJ, CIJ)
C
C---- Provide for final adjustments to CIJ
      call NEST   (X, IX, N, NL, NO, TEX, XNE, XNC, XNU, FCJ, CIJAD,
     $             CHIJ, CIJ)
C
C---- Print
      call PETCIJ (NO, N, NL, CIJ, FCE, KIJ)
C     !END
      call BYE ('SETCIJ')
C
      return
      end
