      subroutine LIMPID
     $(AK,GK,V,XLB,SP,XK,EMUX,F1,NZ,KK,XNUK,XLP,D)
C
C     Rudolf Loeser, 1974 Dec 20
C---- Computes XLP and D, for KNOT.
C     !DASH
      save
C     !DASH
      real*8 AK, CA, CON7, D, EMUX, F1, FAC, GK, SD, SL, SP, V, XK, XKK,
     $       XLB, XLP, XNUK, ZERO
      integer I, K, KK, NZ
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
      external RIGEL, DIVIDE, HI, BYE
C
C               XK(KKX), AK(KKX), GK(KKX), F1(NZ), SP(NZ,KKX), XLP(NZ),
      dimension XK(*),   AK(*),   GK(*),   F1(*),  SP(NZ,*),   XLP(*),
C
C               D(NZ), V(NZ,KKX), XLB(NZ,KKX), EMUX(NZ,KKX)
     $          D(*),  V(NZ,*),   XLB(NZ,*),   EMUX(NZ,*)
C
      call HI ('LIMPID')
C     !BEG
      call RIGEL    (7, CON7)
      CA = (XNUK**3)*CON7
C
      do 101 I = 1,NZ
        SD = ZERO
        SL = ZERO
C
        do 100 K = 1,KK
          XKK = XK(K)
          FAC = (AK(K)*GK(K)*XLB(I,K))/XKK
          SD  = SD+FAC*EMUX(I,K)
          SL  = SL+(V(I,K)*SP(I,K)*FAC)/((XKK**3)*CA)
  100   continue
C
        call DIVIDE (SD, F1(I), D(I))
C
        XLP(I) = SL
  101 continue
C     !END
      call BYE ('LIMPID')
C
      return
      end
