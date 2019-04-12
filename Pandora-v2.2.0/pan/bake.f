      subroutine BAKE
     $(CP,RKI,KK,AK,GK,XK,XJIK,N,KOLEV,DNRT,NCR,KOOL,XNU)
C
C     Rudolf Loeser, 1974 Dec 27
C---- Computes RK for FAKE.
C     !DASH
      save
C     !DASH
      real*8 AK, CON20, CP, DNRT, F, FAC, FRQUNT, GK, PLANCK, RKI, XJIK,
     $       XK, XNU, XNUK
      integer IXP, J, KK, KOLEV, N, NCR
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
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 1),PLANCK)
      equivalence (TUNI( 3),FRQUNT)
C     !DASH
C     !EJECT
      external RIGEL, MOVE1, ZERO1, ARRINC, HI, BYE
C
C               AK(KKX), XK(KKX), GK(KKX), XNU(NSL), CP(NSL+1), RKI(N),
      dimension AK(*),   XK(*),   GK(*),   XNU(*),   CP(*),     RKI(*),
C
C               DNRT(N), XJIK(N,KKX)
     $          DNRT(*), XJIK(N,*)
C
      call HI ('BAKE')
C     !BEG
      call RIGEL    (20, CON20)
      FAC = CP(KOLEV)*CON20
C
      IXP = 4
      if(KOOL) then
        FAC = FAC*PLANCK*((XNUK-XNU(KOLEV))*FRQUNT)
        IXP = 3
      end if
C
      if(NCR.gt.0) then
        call MOVE1  (DNRT, N, RKI)
      else
        call ZERO1  (RKI, N)
      end if
C
      do 100 J = 1,KK
        F = FAC*((AK(J)*GK(J))/(XK(J)**IXP))
        call ARRINC (XJIK(1,J), F, RKI, N)
  100 continue
C     !END
      call BYE ('BAKE')
C
      return
      end
