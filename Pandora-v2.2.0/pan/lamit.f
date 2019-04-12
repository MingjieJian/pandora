      subroutine LAMIT
     $(JU,JL,MTR,IKIJ,ILIJ,NL,KIJ,LIJ)
C
C     Rudolf Loeser, 1991 Sep 09
C---- Sets up KIJ and LIJ, for GECKO.
C     (This is version 2 of LAMIT.)
C     !DASH
      save
C     !DASH
      integer I, IKIJ, ILIJ, JL, JU, JUL, KIJ, LIJ, MTR, NL
C     !DASH
      external INDXUL, HI, BYE
C
C               JL(MUL), IKIJ(NL,NL), KIJ(MUL), ILIJ(MUL), LIJ(MUL),
      dimension JL(*),   IKIJ(NL,*),  KIJ(*),   ILIJ(*),   LIJ(*),
C
C               JU(MUL)
     $          JU(*)
C
C     !BEG
      do 100 I = 1,MTR
        call INDXUL (JU(I),JL(I),JUL)
        KIJ(I) = IKIJ(JU(I),JL(I))
        LIJ(I) = ILIJ(JUL)
  100 continue
C     !END
C
      return
      end
