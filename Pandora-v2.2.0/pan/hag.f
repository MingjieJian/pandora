      subroutine HAG
     $(ITAU,RHOIJ,YBRIJ,NL,KRJ,X,IX,ARHO)
C
C     Rudolf Loeser, 1974 Mar 20
C---- Establishes the set of AIJ(u,l)*RHOIJ(u,l) values, for HAMMER.
C     !DASH
      save
C     !DASH
      real*8 ARHO, RHOIJ, X, YBRIJ
      integer IL, ITAU, IU, IUL, IX, KRJ, NL
C     !DASH
      external INDXUL, ARROW, HI, BYE
C
      dimension X(*), IX(*)
C
C               ARHO(MUL), RHOIJ(N,NT), YBRIJ(N,NT)
      dimension ARHO(*),   RHOIJ(*),    YBRIJ(*)
C
      call HI ('HAG')
C     !BEG
      do 101 IU = 2,NL
        do 100 IL = 1,(IU-1)
          call INDXUL (IU,IL,IUL)
          call ARROW  (ITAU,IU,IL,KRJ,RHOIJ,YBRIJ,X,IX,ARHO(IUL))
  100   continue
  101 continue
C     !END
      call BYE ('HAG')
C
      return
      end
