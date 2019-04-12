      subroutine AVENA
     $(ITAU,IU,IL,N,NL,KRJ,GMI,RHOIJ,KIJ,YBAR,WEIGHT,Z,X,IX,XM,SM,ZM11)
C
C     Rudolf Loeser, 1968 Mar 26
C---- Computes matrix M-prime (XM), the vector script-M (SM), and
C     variable little-m(1,1) (ZM11), for GRASS.
C                                    > Not for VAMOS!
C
C     See also OYSTER & ARSENAL.
C     !DASH
      save
C     !DASH
      real*8 GMI, RHOIJ, SM, WEIGHT, X, XM, YBAR, Z, ZM11
      integer IL, ITAU, IU, IX, KIJ, KRJ, N, NL, NLM
C     !DASH
      external  TRITICM, HORDEUM, PANICUM, HI, BYE
C
      dimension X(*), IX(*)
C
C               YBAR(N,NT), RHOIJ(N,NT), WEIGHT(MUL,NT), XM(NL-1,NL-1),
      dimension YBAR(*),    RHOIJ(*),    WEIGHT(*),      XM(*),
C
C               GMI(N,NSL), SM(NL-1), KIJ(NL,NL), Z(NL,NL)
     $          GMI(*),     SM(*),    KIJ(*),     Z(*)
C
C
      call HI ('AVENA')
C     !BEG
      NLM = NL-1
      call TRITICM (ITAU, IU, IL, N, NL, NLM, KRJ, GMI, RHOIJ, KIJ,
     $              YBAR, WEIGHT, Z, X, IX, XM  )
      call HORDEUM (ITAU, IU, IL, N, NL, NLM, KRJ, GMI, RHOIJ, KIJ,
     $              YBAR, WEIGHT, Z, X, IX, SM  )
      call PANICUM (ITAU, IU, IL, N, NL, NLM, KRJ, GMI, RHOIJ, KIJ,
     $              YBAR, WEIGHT, Z, X, IX, ZM11)
C     !END
      call BYE ('AVENA')
C
      return
      end
