      subroutine PALE
     $(N,KK,CP,AK,GK,XK,V,XJIK,BD1,BD1EP,EP1,EP2,RS,RP,CHECKL,KOLEV)
C
C     Rudolf Loeser, 1974 Dec 26
C---- Computes RP, and computes and prints CHECKL, for ROPE.
C     !DASH
      save
C     !DASH
      real*8 AK, BD1, BD1EP, CHECKL, CON20, CP, EP1, EP2, F, FAC, GK,
     $       ONE, RAT, RP, RPS, RS, V, XJIK, XK
      integer I, K, KK, KOLEV, MO, N
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
C     !EJECT
      external  ZERO1, RIGEL, DIVIDE, GYGES, HI, BYE
C
C               CHECKL(N), AK(KKX), GK(KKX), XK(KKX), V(N,KKX), CP(NL),
      dimension CHECKL(*), AK(*),   GK(*),   XK(*),   V(N,*),   CP(*),
C
C               BD1(N), EP1(N), EP2(N), RP(N), BD1EP(N), XJIK(N,KKX),
     $          BD1(*), EP1(*), EP2(*), RP(*), BD1EP(*), XJIK(N,*),
C
C               RS(N)
     $          RS(*)
C
      call HI ('PALE')
C     !BEG
      call RIGEL    (20, CON20)
      FAC = CP(KOLEV)*CON20
C
      call ZERO1    (RP,N)
      do 101 K = 1,KK
        call DIVIDE ((AK(K)*GK(K)), (XK(K)**4), RAT)
        F = FAC*RAT
        do 100 I=1,N
          RP(I) = RP(I)+F*V(I,K)*XJIK(I,K)
  100   continue
  101 continue
C
      do 102 I=1,N
        call DIVIDE (RP(I), RS(I), RPS)
        call DIVIDE ((ONE+EP1(I)), (RPS+EP2(I)), BD1EP(I))
  102 continue
C
      call GYGES    (MO, N, KOLEV, BD1, BD1EP, CHECKL)
C     !END
      call BYE ('PALE')
C
      return
      end
