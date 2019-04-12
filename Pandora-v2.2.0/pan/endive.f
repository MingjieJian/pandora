      subroutine ENDIVE
     $(N,NL,M,RHOIJ,YBRIJ,X,IX,ARHO,CL1TA,C1LTA)
C
C     Rudolf Loeser, 1976 Nov 26
C---- Computes intermediates for COCOS.
C     (This is version 2 of ENDIVE.)
C     !DASH
      save
C     !DASH
      real*8 ARHO, C1LTA, CL1TA, RHOIJ, X, YBRIJ
      integer I, IX, JL, JU, KRJ, L, M, N, NL
C     !DASH
      external CADDY, ARROW, HI, BYE
C
      dimension X(*), IX(*)
C
C               YBRIJ(N,NT), CL1TA(N,NL), C1LTA(N,NL), RHOIJ(N,NT),
      dimension YBRIJ(*),    CL1TA(N,*),  C1LTA(N,*),  RHOIJ(*),
C
C               ARHO(N,NL)
     $          ARHO(N,*)
C
      data KRJ, JU, JL /1, 0, 0/
C
      call HI ('ENDIVE')
C     !BEG
      do 101 I = 1,N
        do 100 L = 1,NL
          call CADDY (I,L,M,KRJ,JU,JL,YBRIJ,X,IX,CL1TA(I,L))
          call CADDY (I,M,L,KRJ,JU,JL,YBRIJ,X,IX,C1LTA(I,L))
          call ARROW (I,L,M,KRJ,RHOIJ,YBRIJ,X,IX,ARHO(I,L))
  100   continue
  101 continue
C     !END
      call BYE ('ENDIVE')
C
      return
      end
