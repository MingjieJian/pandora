      subroutine BERG
     $(N,KK,U,TE,XK,AK,GK,CP,EMUX,F1,RS)
C
C     Rudolf Loeser, 1975 Sep 26
C---- Computes intermediates for GLACIER.
C     !DASH
      save
C     !DASH
      real*8 AK, CP, EMUX, F1, GK, RS, TE, U, XK, XNUK
      integer KK, KOLEV, N
C     !COM
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 33),KOLEV)
      equivalence (RZQ(  9),XNUK )
C     !DASH
      external HERRING, PUREE, TOPPING, HI, BYE
C
C               GK(KKX), TE(N), CP(NSL), U(N), RS(N), XK(KKX), AK(KKX),
      dimension GK(*),   TE(*), CP(*),   U(*), RS(*), XK(*),   AK(*),
C
C               EMUX(N,KKX), F1(N)
     $          EMUX(*),     F1(*)
C
      call HI ('BERG')
C     !BEG
C---- Compute U
      call HERRING (N, TE, U)
C---- Compute EMUX: exp(-U*XK)
      call PUREE   (U, N, XK, KK, EMUX)
C---- Compute F1 and RS
      call TOPPING (KK, XK, AK, GK, N, EMUX, F1, RS, XNUK, CP, KOLEV)
C     !END
      call BYE ('BERG')
C
      return
      end
