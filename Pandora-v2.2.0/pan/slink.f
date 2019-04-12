      subroutine SLINK
     $(I,N,NL,KRJ,CIJ,PIJ,BDI,RHOIJ,YBRIJ,KIJ,WEIGHT,X,IX,GMI,Z,PE,FE)
C
C     Rudolf Loeser, 1968 Jan 30
C---- Computes PE and FE, for the case NL=2.
C     (This is version 2 of SLINK.)
C     !DASH
      save
C     !DASH
      real*8 BDI, CIJ, FE, GMI, PE, PIJ, RAT, RHOIJ, WEIGHT, X, YBRIJ,
     $       Z
      integer I, IL, IU, IX, KIJ, KRJ, N, NL
C     !DASH
      external ZEA, DIVIDE, HI, BYE
C
      dimension X(*), IX(*)
C
C               CIJ(N,NL**2), PIJ(N,NL**2), KIJ(NL,NL), WEIGHT(MUL,NT),
      dimension CIJ(*),       PIJ(*),       KIJ(*),     WEIGHT(*),
C
C               RHOIJ(N,NT), YBRIJ(N,NT), GMI(N,NSL), BDI(N,NL),
     $          RHOIJ(*),    YBRIJ(*),    GMI(N,*),   BDI(*),
C
C               Z(NL,NL)
     $          Z(NL,*)
C
      data IU,IL /2, 1/
C
      call HI ('SLINK')
C     !BEG
      call ZEA    (I, IU, IL, N, NL, KRJ, CIJ, PIJ, GMI, BDI, RHOIJ,
     $             KIJ, WEIGHT, YBRIJ, X, IX, Z)
C
      call DIVIDE (GMI(I,IL), GMI(I,IU), RAT)
C
      FE = Z(IL,IU)*RAT
      PE = Z(IU,IL)+Z(IU,IU)
C     !END
      call BYE ('SLINK')
C
      return
      end
