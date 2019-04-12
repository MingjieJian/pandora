      subroutine TREATY
     $(IU,IL,NL,NLM,GMI,SAIJ,YBRIJ,Z,X,IX,XM)
C
C     Rudolf Loeser, 2003 Nov 18
C---- Computes XM, the matrix M-prime, for the VAMOS method.
C
C     See also OYSTER & TRITICM.
C     !DASH
      save
C     !DASH
      real*8 GMI, Q, SAIJ, SUM, X, XM, YBRIJ, Z
      integer I, IL, IPO, IU, IX, J, JPO, L, NL, NLM
C     !DASH
      external CUBIT, ROWSUM, HI, BYE
C
      dimension X(*), IX(*)
C
C               YBRIJ(NL,NL), XM(NLM,NLM), GMI(NL), SAIJ(NL,NL), Z(NL,NL)
      dimension YBRIJ(*),     XM(NLM,*),   GMI(*),  SAIJ(*),     Z(NL,*)
C
      call HI ('TREATY')
C     !BEG
      if(NLM.gt.0) then
        do 102 J = 1,NLM
          JPO = J+1
          do 101 I = 1,NLM
            IPO = I+1
            if(I.eq.J) then
C----         Diagonal terms
              call ROWSUM  (Z(IPO,1), 1, NL, 1, NL, SUM)
              do 100 L = 1,I
                call CUBIT (IPO, L  , IU, IL, NL, SAIJ, YBRIJ, X, IX, Q)
                SUM = SUM+Q
  100         continue
            else
C----         Off-diagonal terms
              SUM = Z(JPO,IPO)
              if(I.lt.J) then
                call CUBIT (JPO, IPO, IU, IL, NL, SAIJ, YBRIJ, X, IX, Q)
                SUM = SUM+Q
              end if
              SUM = -SUM
            end if
            XM(I,J) = GMI(JPO)*SUM
  101     continue
  102   continue
      end if
C     !END
      call BYE ('TREATY')
C
      return
      end
