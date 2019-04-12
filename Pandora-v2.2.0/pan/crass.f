      subroutine CRASS
     $(ITAU,N,NL,NSL,MSL,KRJ,PIJ,CIJ,GMI,XND,NOK,RHOIJ,YBAR,X,IX,
     $ Z,XM,XR)
C
C     Rudolf Loeser, 2003 Jun 10
C
C---- Computes the following quantities needed for the basic b-ratios
C     calculation:
C                   Z  - the matrix Z
C                   XM - the matrix M-prime
C                   XR - the vector R-prime.
C
C---- When KRJ=1, RHO with JBAR are the main input;
C     when KRJ=2, only JBAR is the input;
C     when KRJ=3, only RHO is the input.
C
C     (See also GRASS & GLASS.)
C     !DASH
      save
C     !DASH
      real*8 CIJ, GMI, PIJ, RHOIJ, X, XM, XND, XR, YBAR, Z, dummy
      integer ITAU, IX, KRJ, MSL, N, NL, NSL
      logical NOK
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external HALT, BAKER, OYSTER, SECALE, HI, BYE
C
      dimension X(*), IX(*)
C
C               PIJ(N,NL**2), CIJ(N,NL**2), RHOIJ(N,NT), XM(NL-1,NL-1),
      dimension PIJ(*),       CIJ(*),       RHOIJ(*),    XM(*),
C
C               GMI(N,NSL), YBAR(N,NT), Z(NL,NL), XR(NL-1), XND(N,NL)
     $          GMI(*),     YBAR(*),    Z(*),     XR(*),    XND(*)
C     !EJECT
C
      call HI ('CRASS')
C     !BEG
      if((ITAU.lt.1).or.(ITAU.gt.N)) then
        write (MSSLIN(1),100) N,ITAU
  100   format('N =',I12,'; but ITAU =',I12,', which is out of range.')
        call HALT ('CRASS', 1)
      end if
      if((KRJ.lt.1).or.(KRJ.gt.3)) then
        write (MSSLIN(1),101) KRJ
  101   format('KRJ =',I12,', which is neither 1, 2, nor 3.')
        call HALT ('CRASS',1)
      end if
C
C---- Compute Z
      call BAKER  (ITAU, N, NL, KRJ, CIJ, PIJ, YBAR, X, IX, Z)
C
C---- Compute XM, SM and ZM11
      call OYSTER (ITAU, N, NL, (NL-1), KRJ, GMI, RHOIJ, YBAR, Z,
     $             X, IX, XM)
C
C---- Compute XR and SR
      call SECALE (ITAU, N, NL, NSL, MSL, KRJ, GMI, XND, NOK, RHOIJ,
     $             YBAR, Z, X, IX, XR, dummy)
C     !END
      call BYE ('CRASS')
C
      return
      end
