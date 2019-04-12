      subroutine GLASS
     $(IU,IL,NL,PIJ,CIJ,GMI,SAIJ,YIJ,YBRIJ,X,IX,Z,XM,XR,SM,SR,ZM11)
C
C     Rudolf Loeser, 2003 Nov 18
C
C---- Computes the following quantities needed for the statistical
C     equilibrium calculations at a given depth for the VAMOS method:
C
C           Z    - the matrix Z
C           XM   - the matrix M-prime
C           XR   - the vector R-prime
C           SM   - the vector script-M
C           SR   - the variable script-R
C           ZM11 - the variable little-m(1,1).
C
C     See also CRASS & GRASS.
C     !DASH
      save
C     !DASH
      real*8 CIJ, GMI, PIJ, SAIJ, SM, SR, X, XM, XR, YBRIJ, YIJ, Z,
     $       ZM11
      integer IL, IU, IX, NL, NLM
C     !DASH
      external ZIA, TREATY, HARDY, PANICKY, SECULAR, HI, BYE
C
      dimension X(*), IX(*)
C
C               PIJ(NL,NL), YBRIJ(NL,NL), XM(NL-1,NL-1), SAIJ(NL,NL),
      dimension PIJ(*),     YBRIJ(*),       XM(*),         SAIJ(*),
C
C               GMI(NL), CIJ(NL,NL), SM(NL-1), XR(NL-1), YIJ(NL,NL),
     $          GMI(*),  CIJ(*),     SM(*),    XR(*),    YIJ(*),
C
C               Z(NL,NL)
     $          Z(*)
C
      call HI ('GLASS')
C     !BEG
C---- Compute Z
      call ZIA     (IU, IL, NL, CIJ, PIJ, YIJ, YBRIJ, X, IX, Z)
C
      NLM = NL-1
C---- Compute XM
      call TREATY  (IU, IL, NL, NLM, GMI, SAIJ, YBRIJ, Z, X, IX, XM)
C---- Compute SM
      call HARDY   (IU, IL, NL, NLM, GMI, SAIJ, YBRIJ, Z, X, IX, SM)
C---- Compute ZM11
      call PANICKY (IU, IL, NL,      GMI, SAIJ, YBRIJ, Z, X, IX, ZM11)
C---- Compute XR and SR
      call SECULAR (NL, GMI, Z, XR, SR)
C     !END
      call BYE ('GLASS')
C
      return
      end
