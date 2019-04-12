      subroutine HARDY
     $(IU,IL,NL,NLM,GMI,SAIJ,YBRIJ,Z,X,IX,SM)
C
C     Rudolf Loeser, 2003 Nov 18
C---- Computes SM, the vector script-M, for the VAMOS method.
C
C     See also HORDEUM.
C     !DASH
      save
C     !DASH
      real*8 GMI, Q, SAIJ, SM, X, YBRIJ, Z
      integer IL, IU, IX, J, JPO, NL, NLM
C     !DASH
      external CUBIT, HI, BYE
C
      dimension X(*), IX(*)
C
C               SM(NLM), SAIJ(NL,NL), GMI(NL), YBRIJ(NL,NL), Z(NL,NL)
      dimension SM(*),   SAIJ(*),     GMI(*),  YBRIJ(*),     Z(NL,*)
C
      call HI ('HARDY')
C     !BEG
      if(NLM.gt.0) then
        do 100 J = 1,NLM
          JPO = J+1
          call CUBIT (JPO, 1, IU, IL, NL, SAIJ, YBRIJ, X, IX, Q)
          SM(J) = GMI(JPO)*(Z(JPO,1)+Q)
  100   continue
      end if
C     !END
      call BYE ('HARDY')
C
      return
      end
