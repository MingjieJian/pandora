      subroutine AFRICA
     $(ITAU,N,NL,RHOIJ,YBRIJ,CIJ,GMI,CQSI,RKI,CKI,KDGV,GVL,X,IX,XM)
C
C     Rudolf Loeser, 1968 Jun 11
C---- Computes the matrix XM, for AMERICA.
C     !DASH
      save
C     !DASH
      real*8 ARHO, CIJ, CKI, CQSI, GMI, GVL, RHOIJ, RKI, T, TUC, X, XM,
     $       YBRIJ, ZERO
      integer I, IL, ITAU, IX, J, JI, JL, JU, K, KDGV, KRJ, L, N, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external INDXIJ, CADDY, ARROW, HI, BYE
C
      dimension X(*), IX(*)
C
C               GVL(N,NL), CQSI(N,NL), RKI(N,NL), CKI(N,NL), XM(NL,NL),
      dimension GVL(N,*),  CQSI(N,*),  RKI(N,*),  CKI(N,*),  XM(NL,*),
C
C               CIJ(N,NL**2), RHOIJ(N,NT), YBRIJ(N,NT), GMI(N,NSL)
     $          CIJ(N,*),     RHOIJ(*),    YBRIJ(*),    GMI(N,*)
C
      data KRJ,JU,JL /1,0,0/
C     !EJECT
C
      call HI ('AFRICA')
C     !BEG
      XM(1,1) = ZERO
C
      do 100 K = 2,NL
        XM(K,1) = -CQSI(ITAU,K)/GMI(ITAU,1)
        XM(1,K) = -RKI(ITAU,K)-CKI(ITAU,K)
        if(KDGV.ne.0) then
          XM(1,K) = XM(1,K)-GVL(ITAU,K)
        end if
  100 continue
C
      do 104 I = 2,NL
        do 103 J = 2,NL
          if(I.ne.J) then
            call CADDY      (ITAU,J,I,KRJ,JU,JL,YBRIJ,X,IX,TUC)
            call INDXIJ      (J,I,JI)
            T = -(CIJ(ITAU,JI)+TUC)
          end if
          if(I.lt.J) then
            call ARROW      (ITAU,J,I,KRJ,RHOIJ,YBRIJ,X,IX,ARHO)
            T = T-ARHO
          else if(I.eq.J) then
            T = RKI(ITAU,I)+CKI(ITAU,I)
            if(KDGV.ne.0) then
              T = T+GVL(ITAU,I)
            end if
            do 101 L=1,NL
              if(L.ne.I) then
                call CADDY  (ITAU,I,L,KRJ,JU,JL,YBRIJ,X,IX,TUC)
                call INDXIJ (I,L,IL)
                T = T+(CIJ(ITAU,IL)+TUC)
              end if
  101       continue
            do 102 L = 1,(I-1)
              call ARROW    (ITAU,I,L,KRJ,RHOIJ,YBRIJ,X,IX,ARHO)
              T = T+ARHO
  102       continue
          end if
          XM(I,J) = T
  103   continue
  104 continue
C     !END
      call BYE ('AFRICA')
C
      return
      end
