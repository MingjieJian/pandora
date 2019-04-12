      subroutine MALE
     $(CP,RL,KK,AK,GK,XK,XJIK,N,KOLEV,EMUX,KOOL,XNU)
C
C     Rudolf Loeser, 1979 Oct 19
C---- Computes RL, for RALE.
C     !DASH
      save
C     !DASH
      real*8 A, AK, CA, CON13, CON20, CON7, CP, EMUX, EX, F, FAC, GK,
     $       RL, X3, XJIK, XK, XNU, XNUK
      integer I, K, KK, KOLEV, N
      logical KOOL
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
      equivalence (RZQ(  9),XNUK )
C     !DASH
C     !EJECT
      external RIGEL, ZERO1, HI, BYE
C
C               CP(NSL+1), EMUX(N,KKX), XK(KKX), XNU(NSL), XJIK(N,KKX),
      dimension CP(*),     EMUX(N,*),   XK(*),   XNU(*),   XJIK(N,*),
C
C               AK(KKX), GK(KKX), RL(N)
     $          AK(*),   GK(*),   RL(*)
C
      call HI ('MALE')
C     !BEG
      call ZERO1   (RL, N)
      call RIGEL   (7,  CON7)
      call RIGEL   (20, CON20)
C
      CA  = (XNUK**3)*CON7
      FAC = CP(KOLEV)*CON20
C
      if(KOOL) then
        call RIGEL (13, CON13)
        FAC = FAC*(XNUK-XNU(KOLEV))*CON13
      end if
C
      do 101 K = 1,KK
C
        X3 = XK(K)**3
        A  = CA*X3
        F  = ((AK(K)*GK(K))/(XK(K)*X3))*FAC
        if(KOOL) then
          F = F*XK(K)
        end if
C
        do 100 I = 1,N
          RL(I) = RL(I)+F*EMUX(I,K)*(A+XJIK(I,K))
  100   continue
C
  101 continue
C     !END
      call BYE ('MALE')
C
      return
      end
