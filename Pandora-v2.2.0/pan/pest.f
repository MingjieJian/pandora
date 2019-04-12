      subroutine PEST
     $(ITAU,N,NL,RKI,CKI,CQSI,SQS,SLT,GVL,KDGV,PIS,AL,PIJ)
C
C     Rudolf Loeser, 1978 Nov 14
C---- Recomputes PIJ at depth ITAU.
C     !DASH
      save
C     !DASH
      real*8 AL, CKI, CQSI, GVL, ONE, PIJ, PIS, RHJ, RKI, SLT, SQS, TRM
      integer I, IJ, ITAU, J, KDGV, N, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external INDXIJ, JEST, DIVIDE, HI, BYE
C
C               RKI(N,NL), CKI(N,NL), CQSI(N,NL), AL(NL), PIJ(N,NL**2),
      dimension RKI(N,*),  CKI(N,*),  CQSI(N,*),  AL(*),  PIJ(N,*),
C
C               GVL(N,NL), PIS(N,NL), SQS(N), SLT(N)
     $          GVL(N,*),  PIS(N,*),  SQS(*), SLT(*)
C
      call HI ('PEST')
C     !BEG
      do 101 J = 1,NL
C
        do 100 I = 1,NL
C
          call INDXIJ   (I,J,IJ)
C
          if(I.ne.J) then
            call DIVIDE (CQSI(ITAU,J), (SQS(ITAU)+SLT(ITAU)), RHJ)
            call JEST   (RKI(ITAU,I), CKI(ITAU,I), GVL(ITAU,I), KDGV,
     $                   TRM)
            PIJ(ITAU,IJ) = TRM*RHJ + AL(J)*PIS(ITAU,I)
C
          else
            PIJ(ITAU,IJ) = ONE
          end if
C
  100   continue
C
  101 continue
C     !END
      call BYE ('PEST')
C
      return
      end
