      subroutine LAUGH
     $(AK,GK,XK,KK,XNUK,XLB,XLP,SP,V,WN,IXUSE,NE,NZ,XLF)
C
C     Rudolf Loeser, 1974 Dec 19
C---- Computes XLF, for TAURUS.
C
C     (Note: NZ equals N; NE (i.e. "N to eta") is the length of a
C     reduced set, not greater than N.)
C     !DASH
      save
C     !DASH
      real*8 AK, CA, CON7, GK, RAT, SP, V, WN, XK, XLB, XLF, XLP, XNUK
      integer IXUSE, K, KK, NE, NZ
C     !DASH
      external MOVE1, RIGEL, DIVIDE, CHINK, HI, BYE
C
C               XK(KKX), XLP(NZ), WN(NZ,NZ,KKX), V(NZ,KKX), IXUSE(KKX),
      dimension XK(*),   XLP(*),  WN(NZ,NZ,*),   V(NZ,*),   IXUSE(*),
C
C               SP(NZ,KKX), XLF(NZ), AK(KKX), GK(KKX), XLB(NZ,KKX)
     $          SP(NZ,*),   XLF(*),  AK(*),   GK(*),   XLB(NZ,*)
C
      call HI ('LAUGH')
C     !BEG
      call MOVE1      (XLP, NE, XLF)
C
      call RIGEL      (7, CON7)
      CA = (XNUK**3)*CON7
C
      do 100 K = 1,KK
        if(IXUSE(K).eq.1) then
          call DIVIDE ((AK(K)*GK(K)), (CA*(XK(K)**4)), RAT)
          call CHINK  (NE, RAT, XLB(1,K), SP(1,K), V(1,K), WN(1,1,K),
     $                 XLF)
        end if
  100 continue
C     !END
      call BYE ('LAUGH')
C
      return
      end
