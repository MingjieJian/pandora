      subroutine CHANT
     $(N,NL,M,RHOIJ,YBRIJ,GMI,X,IX,ARHO,CINC,FLM)
C
C     Rudolf Loeser, 1976 Aug 31
C---- Computes intermediates for ALTAR.
C     !DASH
      save
C     !DASH
      real*8 ARHO, CINC, FLM, GMI, GMIM, RHOIJ, X, YBRIJ, ZERO
      integer I, IX, J, JL, JU, K, KRJ, L, M, N, NL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external ARROW, CADDY, HI, BYE
C
      dimension X(*), IX(*)
C
C               ARHO(N,NL), CINC(N,NL), GMI(N,NSL), YBRIJ(N,NT),
      dimension ARHO(N,*),  CINC(N,*),  GMI(N,*),   YBRIJ(*),
C
C               RHOIJ(N,NT), FLM(N,NL)
     $          RHOIJ(*),    FLM(N,*)
C
      data KRJ, JU, JL /1, 0, 0/
C     !EJECT
C
      call HI ('CHANT')
C     !BEG
      do 101 I = 1,N
        GMIM = GMI(I,M)
        do 100 L = 1,NL
C
          if(L.eq.M) then
            ARHO(I,L) = ZERO
            CINC(I,L) = ZERO
          else
            if(L.lt.M) then
              J = M
              K = L
            else
              J = L
              K = M
            end if
            call ARROW (I,J,K,KRJ,RHOIJ,YBRIJ,X,IX,ARHO(I,L))
            call CADDY (I,K,J,KRJ,JU,JL,YBRIJ,X,IX,CINC(I,L))
          end if
C
          FLM(I,L) = (GMI(I,L)/GMIM)*ARHO(I,L)
C
  100   continue
  101 continue
C     !END
      call BYE ('CHANT')
C
      return
      end
