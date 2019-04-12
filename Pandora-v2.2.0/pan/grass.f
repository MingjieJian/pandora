      subroutine GRASS
     $(ITAU,IU,IL,N,NL,NSL,MSL,KRJ,PIJ,CIJ,GMI,XND,NOK,BDI,RHOIJ,
     $ KIJ,YBAR,WEIGHT,X,IX,Z,XM,XR,SM,SR,ZM11)
C
C     Rudolf Loeser, 1971 Dec 16
C
C---- Computes the following quantities needed for the statistical
C     equilibrium calculations:
C           Z    - the matrix Z
C           XM   - the matrix M-prime
C           XR   - the vector R-prime
C           SM   - the vector script-M
C           SR   - the variable script-R
C           ZM11 - the variable little-m(1,1).
C
C           > Not for VAMOS!
C
C     (See also CRASS & GLASS.)
C     !DASH
      save
C     !DASH
      real*8 BDI, CIJ, GMI, PIJ, RHOIJ, SM, SR, WEIGHT, X, XM, XND, XR,
     $       YBAR, Z, ZM11
      integer IL, ITAU, IU, IX, KIJ, KRJ, MSL, N, NL, NSL
      logical NOK
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, ZEA, AVENA, SECALE, HI, BYE
C
      dimension X(*), IX(*)
C
C               PIJ(N,NL**2), CIJ(N,NL**2), RHOIJ(N,NT), XM(NL-1,NL-1),
      dimension PIJ(*),       CIJ(*),       RHOIJ(*),    XM(*),
C
C               BDI(N,NL), GMI(N,NSL), YBAR(N,NT), XND(N,NL), SM(NL-1),
     $          BDI(*),    GMI(*),     YBAR(*),    XND(*),    SM(*),
C
C               WEIGHT(MUL,NT), KIJ(NL,NL), XR(NL-1), Z(NL,NL)
     $          WEIGHT(*),      KIJ(*),     XR(*),    Z(*)
C     !EJECT
C
      call HI ('GRASS')
C     !BEG
      if((ITAU.lt.1).or.(ITAU.gt.N)) then
        write (MSSLIN(1),100) N,ITAU
  100   format('N =',I12,'; but ITAU =',I12,', which is out of range.')
        call HALT ('GRASS', 1)
      end if
      if((KRJ.ne.1).and.(KRJ.ne.3)) then
        write (MSSLIN(1),101) KRJ
  101   format('KRJ =',I12,', which is neither 1 or 3.')
        call HALT ('GRASS', 1)
      end if
C
C---- Compute Z
      call ZEA    (ITAU, IU, IL, N, NL, KRJ, CIJ, PIJ, GMI, BDI, RHOIJ,
     $             KIJ, WEIGHT, YBAR, X, IX, Z)
C
C---- Compute XM, SM and ZM11
      call AVENA  (ITAU, IU, IL, N, NL, KRJ, GMI, RHOIJ, KIJ, YBAR,
     $             WEIGHT, Z, X, IX, XM, SM, ZM11)
C
C---- Compute XR and SR
      call SECALE (ITAU, N, NL, NSL, MSL, KRJ, GMI, XND, NOK, RHOIJ,
     $             YBAR, Z, X, IX, XR, SR)
C     !END
      call BYE ('GRASS')
C
      return
      end
