      subroutine TOPPING
     $(KK,XK,AK,GK,N,EMUX,F1,RS,XNUK,CP,KOLEV)
C
C     Rudolf Loeser, 1974 Dec 06
C---- Computes F1 and RS, for the Lyman calculation.
C     !DASH
      save
C     !DASH
      real*8 AK, CON20, CON7, CP, EMUX, F1, FAC, GK, RS, RSF, SUM, TERM,
     $       XK, XNUK, ZERO
      integer I, J, KK, KOLEV, N
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
C               XK(KKX), AK(KKX), GK(KKX), F1(N), EMUX(N,KKX), CP(NSL),
      dimension XK(*),   AK(*),   GK(*),   F1(*), EMUX(N,*),   CP(*),
C
C               RS(N)
     $          RS(*)
C
      call HI ('TOPPING')
C     !BEG
      call RIGEL      (7,  CON7 )
      call RIGEL      (20, CON20)
      FAC = CON7*(XNUK**3)
      RSF = CON20*CP(KOLEV)*FAC
C
      do 101 I = 1,N
        SUM = ZERO
C
        do 100 J = 1,KK
          call DIVIDE ((AK(J)*GK(J)*EMUX(I,J)), XK(J), TERM)
          SUM = SUM+TERM
  100   continue
C
        F1(I) = SUM
        RS(I) = SUM*RSF
  101 continue
C     !END
      call BYE ('TOPPING')
C
      return
      end
