      subroutine OYSTER
     $(ITAU,N,NL,NLM,KRJ,GMI,RHOIJ,YBAR,Z,X,IX,XM)
C
C     Rudolf Loeser, 2003 Jun 10
C---- Computes XM, the matrix M-prime, for CRASS.
C     (See also AVENA - TRITICM.)
C     (This is version 3 of OYSTER.)
C     !DASH
      save
C     !DASH
      real*8 ARHO, GMI, RHOIJ, SUM, X, XM, YBAR, Z
      integer I, IPO, ITAU, IX, J, JPO, KRJ, L, N, NL, NLM
C     !DASH
      external ARROW, ROWSUM, HI, BYE
C
      dimension X(*), IX(*)
C
C               YBAR(N,NT), XM(NLM,NLM), RHOIJ(N,NT), GMI(N,NSL),
      dimension YBAR(*),    XM(NLM,*),   RHOIJ(*),    GMI(N,*),
C
C               Z(NL,NL)
     $          Z(NL,*)
C
      call HI ('OYSTER')
C     !BEG
      if(NLM.gt.0) then
        do 102 J = 1,NLM
          JPO = J+1
          do 101 I = 1,NLM
            IPO = I+1
            if(I.eq.J) then
C----         Diagonal terms
              call ROWSUM    (Z(IPO,1), 1, NL, 1, NL, SUM)
              do 100 L = 1,I
                call ARROW (ITAU, IPO, L  , KRJ, RHOIJ, YBAR, X, IX,
     $                      ARHO)
                SUM = SUM+ARHO
  100         continue
            else
C----         Off-diagonal terms
              SUM = -Z(JPO,IPO)
              if(I.lt.J) then
                call ARROW (ITAU, JPO, IPO, KRJ, RHOIJ, YBAR, X, IX,
     $                      ARHO)
                SUM = SUM-ARHO
              end if
            end if
            XM(I,J) = GMI(ITAU,JPO)*SUM
  101     continue
  102   continue
      end if
C     !END
      call BYE ('OYSTER')
C
      return
      end
