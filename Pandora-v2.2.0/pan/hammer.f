      subroutine HAMMER
     $(IU,IL,N,NL,NSL,KRJ,LAST,X,IX,ARHO,Z,GM,XND,BD,PE,FE,DUMP)
C
C     Rudolf Loeser, 1974 Mar 21
C---- Computes statistical equilibrium terms.
C     !DASH
      save
C     !DASH
      real*8 ARHO, BD, FE, GM, PE, VL, VLS, VU, X, XL, XND, XU, Z
      integer IL, ITAU, IU, IX, KRJ, MSL, N, NL, NSL
      logical DMPI, DUMP, KILROY, LAST, NOK
C     !DASH
      external HAM, HEP, HIM, HER, HAT, BUR, ANKE, HIS, SKULL, MASHED,
     $         HI, BYE
C
      dimension X(*), IX(*)
C
C               XND(NL), BD(NL), ARHO(NL*(NL-1)/2), Z(NL,NL), GM(NL),
      dimension XND(*),  BD(*),  ARHO(*),           Z(*),     GM(*),
C
C               PE(N), FE(N)
     $          PE(*), FE(*)
C
      data MSL /1/
C     !EJECT
C
      call HI ('HAMMER')
C     !BEG
      KILROY = .true.
      do 100 ITAU = 1,N
        call SKULL  (DUMP, KILROY, 'HAMMER', 'CHAIN', IU, IL, ITAU,
     $               DMPI)
C
C----   Get A*RHO for this depth
        call HAM    (ITAU, NL, KRJ, X, IX, ARHO)
C----   Get Z for this depth
        call HEP    (ITAU, IU, IL, N, NL, KRJ, LAST, X, IX, Z)
C----   Get other parameters for this depth
        call HIM    (ITAU, N, NL, MSL, X, GM, XND, NOK, BD)
C----   Get XU,XL,VU,VL
        call HER    (IU-1, IL, IU, IU, IL, NL, ARHO, Z, XU)
        call HER    (IL-1, IU, IL, IU, IL, NL, ARHO, Z, XL)
        call HAT    (IU+1, IL, IU, IU, IL, NL, ARHO, Z, GM, BD, VU)
        call HAT    (IL+1, IU, IL, IU, IL, NL, ARHO, Z, GM, BD, VL)
C----   Provide for supplementary levels
        call BUR    (IU, IL, NL, NSL, MSL, ARHO, XND, NOK, VLS, VL)
C
C----   Get PE and FE
        call ANKE   (Z, XU, XL, VU, VL, GM, IU, IL, NL,
     $               PE(ITAU), FE(ITAU))
C
        if(DMPI) then
C----     Dump XU, XL, VU, VL
          call HIS  (XU, XL, VU, VL, Z, GM, XND, BD, VLS, NL, NSL)
        end if
  100 continue
      if(.not.KILROY) then
        call MASHED ('HAMMER')
      end if
C     !END
      call BYE ('HAMMER')
C
      return
      end
