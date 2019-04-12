      subroutine LASSO
     $(IU,IL,NL,YBRIJ,ALF,RATIO)
C
C     Rudolf Loeser, 2003 Nov 18
C---- Computes Jbar/alpha for transition (IU/IL).
C     !DASH
      save
C     !DASH
      real*8 ALF, RATIO, YBRIJ
      integer IL, IU, IUL, NL
C     !DASH
      external INDXUL, DIVIDE, HI, BYE
C
C               YBRIJ(NL,NL), ALF(MUL)
      dimension YBRIJ(NL,*),  ALF(*)
C
      call HI ('LASSO')
C     !BEG
      call INDXUL  (IU, IL, IUL)
      call DIVIDE  (YBRIJ(IU,IL), ALF(IUL), RATIO)
C     !END
      call BYE ('LASSO')
C
      return
      end
