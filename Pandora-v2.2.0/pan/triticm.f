      subroutine TRITICM
     $(ITAU,IU,IL,N,NL,NLM,KRJ,GMI,RHOIJ,KIJ,YBAR,WEIGHT,Z,X,IX,XM)
C
C     Rudolf Loeser, 1968 Mar 26
C---- Computes XM, the matrix M-prime, for AVENA.
C                                      Not for VAMOS!
C
C     See also OYSTER & TREATY.
C     !DASH
      save
C     !DASH
      real*8 ARHO, GMI, RHOIJ, SUM, UU, WEIGHT, X, XM, YBAR, Z, ZERO
      integer I, IL, IPO, ITAU, IU, IX, J, JPO, KIJ, KRJ, L, N, NL, NLM
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  BAMBUSA, ARROW, ROWSUM, HI, BYE
C
      dimension X(*), IX(*)
C
C               YBAR(N,NT), XM(NLM,NLM), RHOIJ(N,NT), WEIGHT(MUL,NT),
      dimension YBAR(*),    XM(NLM,*),   RHOIJ(*),    WEIGHT(*),
C
C               GMI(N,NSL), KIJ(NL,NL), Z(NL,NL)
     $          GMI(N,*),   KIJ(*),     Z(NL,*)
C     !EJECT
C
      call HI ('TRITICM')
C     !BEG
      if(NLM.gt.0) then
C
        do 102 J = 1,NLM
          JPO = J+1
          do 101 I = 1,NLM
            IPO = I+1
C
            if(I.eq.J) then
C----         Diagonal terms
              call ROWSUM    (Z(IPO,1), 1, NL, 1, NL, SUM)
              do 100 L = 1,I
                call BAMBUSA (IPO, L  , IU, IL, WEIGHT, KIJ, NL, UU)
                if(UU.ge.ZERO) then
                  call ARROW (ITAU, IPO, L  , KRJ, RHOIJ, YBAR, X, IX,
     $                        ARHO)
                  SUM = SUM+ARHO
                end if
  100         continue
            else
C----         Off-diagonal terms
              SUM = -Z(JPO,IPO)
              if(I.lt.J) then
                call BAMBUSA (JPO, IPO, IU, IL, WEIGHT, KIJ, NL, UU)
                if(UU.ge.ZERO) then
                  call ARROW (ITAU, JPO, IPO, KRJ, RHOIJ, YBAR, X, IX,
     $                        ARHO)
                  SUM = SUM-ARHO
                end if
              end if
            end if
C
            XM(I,J) = GMI(ITAU,JPO)*SUM
C
  101     continue
  102   continue
      end if
C     !END
      call BYE ('TRITICM')
C
      return
      end
