      subroutine EUROPE
     $(ITAU,N,NL,YBRIJ,CIJ,GMI,CQSI,RLI,CKI,X,IX,D1L,DL1,XM,
     $ EP1,EP2,GVL,KDGV,DMPI)
C
C     Rudolf Loeser, 1968 Jun 12
C---- Computes EP1 and EP2, for AMERICA.
C     !DASH
      save
C     !DASH
      real*8 C, CIJ, CKI, CQSI, D1L, DL1, EP1, EP2, F, G, GMI, GVL, O1L,
     $       OL1, ONE, OOR, R1L, RL1, RLI, SC, SCT, SM, SMT, SQ, SQG,
     $       TUC, X, XM, YBRIJ, ZERO
      integer ITAU, IX, JL, JU, KC, KDGV, KRJ, L, N, NL
      logical DMPI
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external CADDY, INDXIJ, DIVIDE, SIBERIA, CONGO, HI, BYE
C
      dimension X(*), IX(*)
C
C               RLI(N,NSL), CKI(N,NSL), GMI(N,NSL), GVL(N,NL), DL1(NL),
      dimension RLI(N,*),   CKI(N,*),   GMI(N,*),   GVL(N,*),  DL1(*),
C
C               CIJ(N,NL**2), XM(NL,NL), CQSI(N,NSL), YBRIJ(N,NT),
     $          CIJ(N,*),     XM(NL,*),  CQSI(N,*),   YBRIJ(*),
C
C               D1L(NL)
     $          D1L(*)
C
      data KRJ, JU, JL /1, 0, 0/
C
      call HI ('EUROPE')
C     !BEG
      call DIVIDE      (ONE, D1L(1), O1L)
      call DIVIDE      (ONE, DL1(1), OL1)
      SC = ZERO
      SM = ZERO
      SQ = ZERO
      F  = ONE
      do 100 L = 2,NL
        call CADDY     (ITAU, 1, L, KRJ, JU, JL, YBRIJ, X, IX, TUC)
        call INDXIJ    (1, L, KC)
        F   = -F
        SQ  = SQ+CQSI(ITAU,L)
        R1L = D1L(L)*O1L
        SMT = F*XM(1,L)*R1L
        SM  = SM+SMT
        RL1 = DL1(L)*OL1
        SCT = F*(CIJ(ITAU,KC)+TUC)*RL1
        SC  = SC+SCT
        if(DMPI) then
          call SIBERIA (L, CQSI(ITAU,L), R1L, SMT, RL1, SCT)
        end if
  100 continue
      call DIVIDE      (ONE, RLI(ITAU,1), OOR)
      call DIVIDE      (SQ, GMI(ITAU,1), SQG)
      if(KDGV.ne.0) then
        G = GVL(ITAU,1)
      else
        G = ZERO
      end if
      C = CKI(ITAU,1)
      EP1 = (C+SM+SQG  )*OOR
      EP2 = (C+SC    +G)*OOR
C
      if(DMPI) then
        call CONGO     (SQ, SM, EP1, SC, G, EP2)
      end if
C     !END
      call BYE ('EUROPE')
C
      return
      end
