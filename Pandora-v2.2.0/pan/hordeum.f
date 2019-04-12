      subroutine HORDEUM
     $(ITAU,IU,IL,N,NL,NLM,KRJ,GMI,RHOIJ,KIJ,YBAR,WEIGHT,Z,X,IX,SM)
C
C     Rudolf Loeser, 1968 Mar 26
C---- Computes SM, the vector script-M, for AVENA.
C                                       Not for VAMOS!
C
C     See also HARDY.
C     !DASH
      save
C     !DASH
      real*8 ARHO, GMI, RHOIJ, SM, UU, WEIGHT, X, YBAR, Z, ZERO
      integer IL, ITAU, IU, IX, J, JPO, KIJ, KRJ, N, NL, NLM
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external  BAMBUSA, ARROW, HI, BYE
C
      dimension X(*), IX(*)
C
C               SM(NLM), KIJ(NL,NL), Z(NL,NL), RHOIJ(N,NT), YBAR(N,NT),
      dimension SM(*),   KIJ(*),     Z(NL,*),  RHOIJ(*),    YBAR(*),
C
C               WEIGHT(MUL,NT), GMI(N,NL)
     $          WEIGHT(*),      GMI(N,*)
C
C
      call HI ('HORDEUM')
C     !BEG
      if(NLM.gt.0) then
        do 100 J = 1,NLM
          JPO = J+1
          call BAMBUSA (JPO, 1, IU, IL, WEIGHT, KIJ, NL, UU)
          if(UU.ne.ZERO) then
            ARHO = ZERO
          else
            call ARROW (ITAU, JPO, 1, KRJ, RHOIJ, YBAR, X, IX, ARHO)
          end if
          SM(J) = GMI(ITAU,JPO)*(Z(JPO,1)+ARHO)
  100   continue
      end if
C     !END
      call BYE ('HORDEUM')
C
      return
      end
