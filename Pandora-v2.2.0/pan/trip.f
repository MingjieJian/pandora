      subroutine TRIP
     $(X,IX,W,IW,N,NL,NSL,K,L,KRJ,PE,FE,GMI,XND,CIJ,PIJ,RHO,YBR,KIJ,
     $ WEIGHT,BDI,Z,XM,XR,SM,DI,XS,DUMP)
 
C     Rudolf Loeser, 1968 Jan 30
C---- Computes Statistical Equilibrium terms PE and FE,
C     for the NOVA method.
C
C     See also TEACUP.
C     !DASH
      save
C     !DASH
      real*8 BDI, CIJ, D, DI, FD, FE, GMI, ONE, OOG, PE, PIJ, RHO, SM,
     $       W, WEIGHT, X, XM, XND, XR, XS, XX, YBR, YY, Z, ZM11
      integer I, IW, IX, K, KIJ, KRJ, L, LGT, MSL, N, NL, NLM, NSL
      logical DMPI, DUMP, KILROY, NLQ, NOK
      character TITLE*50
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external DIVIDE, SLINK, TROT, GAG, TRUE, SKULL, MASHED, PLUSD,
     $         HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               XM(NL-1,NL-1), WEIGHT(MUL,NT), XS(NL-1,NL-1), SM(NL-1),
      dimension XM(*),         WEIGHT(*),      XS(*),         SM(*),
C
C               PE(N), FE(N), GMI(N,NSL), XR(NL-1), Z(NL,NL), DI(NL-1),
     $          PE(*), FE(*), GMI(N,*),   XR(*),    Z(*),     DI(*),
C
C               CIJ(N,NL**2), YBR(N,NT), PIJ(N,NL**2), RHO(N,NT),
     $          CIJ(*),       YBR(*),    PIJ(*),       RHO(*),
C
C               KIJ(NL,NL), BDI(N,NL), XND(N,NL)
     $          KIJ(*),     BDI(*),    XND(N,*)
C
      data MSL /1/
C
      call HI ('TRIP')
C     !BEG
      call PLUSD      (XND(1,MSL), 1, N, LGT)
      NOK = LGT.eq.N
C
      NLQ = NL.le.2
      NLM = NL-1
C     !EJECT
C---- Loop over all depths
      KILROY = .true.
      do 101 I = 1,N
        if(NLQ) then
C----     Take simple way out if NL=2
          call SLINK  (I, N, NL, KRJ, CIJ, PIJ, BDI, RHO, YBR, KIJ,
     $                 WEIGHT, X, IX, GMI, Z, PE(I), FE(I))
        else
C
C----     No, go the full route
          call SKULL  (DUMP, KILROY, 'TRIP', 'NOVA', K, L, I, DMPI)
C
C----     Get modified matrices and little-M11
          call TROT   (XM, XR, SM, I, K, L, N, NL, NLM, NSL, MSL, KRJ,
     $                 GMI, XND, NOK, CIJ, PIJ, RHO, YBR, KIJ, WEIGHT,
     $                 BDI, Z, ZM11, X, IX, DMPI)
C----     Get determinants
          call GAG    (NL, XM, D, DI, XS, TITLE, W, IW)
C----     Get XX and YY
          call TRUE   (DI, XX, YY, XM, XR, NLM, D, ZM11, W, IW, I, DMPI)
C
C----     And now compute PE and FE
          call DIVIDE (ONE, GMI(I,K), OOG)
          PE(I) = XX*OOG
          FE(I) = YY*OOG
        end if
  101 continue
      if(.not.KILROY) then
        call MASHED   ('TRIP')
      end if
C     !END
      call BYE ('TRIP')
C
      return
      end
