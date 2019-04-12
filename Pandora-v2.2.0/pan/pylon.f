      subroutine PYLON
     $(X,IX,W,IW,RHO,YBR,RL,RK,CK,CIJ,GM,BDI,CQS,XND,CQTAIL,AL,GVL,
     $ EP1N,EP2N,T,SW,RS,TAUK)
C
C     Rudolf Loeser, 1981 Dec 14
C---- Supervises "Lyman" EP1, EP2 calculations.
C     !DASH
      save
C     !DASH
      real*8 AL, BDI, CIJ, CK, CQS, CQTAIL, EP1N, EP2N, GM, GVL, RHO,
     $       RK, RL, RS, T, TAUK, W, X, XND, YBR
      integer IW, IX, KDGV, KOLEV, METEP, N, NL
      logical SW
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
      equivalence (JZQ( 1),N  )
C
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (KZQ( 25),METEP)
      equivalence (KZQ( 33),KOLEV)
C
C---- MISC        as of 2007 Jan 18
      real*8      REST
      integer     LEST
      character   QEST*8
      dimension   REST(7),LEST(82),QEST(1)
      common      /MISC1/ REST
      common      /MISC2/ LEST
      common      /MISC3/ QEST
C     Collections of (mostly) dynamic parameters.
      equivalence (LEST(39),KDGV )
C     !DASH
C     !EJECT
      external ARCTIC, PUNGY, TEMPLE, FOREST, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               RHO(N,NT), RL(N,NSL), RK(N,NSL), CIJ(N,NL,NL), TAUK(N),
      dimension RHO(*),    RL(*),     RK(*),     CIJ(*),       TAUK(*),
C
C               RS(N), BDI(N,NL), YBR(N,NT), XND(N,NL), T(4), CK(N,NL),
     $          RS(*), BDI(*),    YBR(*),    XND(*),    T(*), CK(*),
C
C               EP2N(N,4), GVL(N,NL), GM(N,NSL), EP1N(N,4), CQS(N,NSL),
     $          EP2N(N,*), GVL(*),    GM(*),     EP1N(N,*), CQS(*),
C
C               AL(NL), CQTAIL(MQT), SW(4)
     $          AL(*),  CQTAIL(*),   SW(*)
C
      call HI ('PYLON')
C     !BEG
      if(SW(1)) then
C----   NOVA
        call ARCTIC (EP1N(1,1), EP2N(1,1), T(1), RHO, YBR, RL, RK, CK,
     $               CIJ, GM, CQS, GVL, KDGV, X, IX, W, IW)
      end if
C
      if(SW(2)) then
C----   COMPLEX-UPPPER
        call PUNGY  (EP1N(1,2), EP2N(1,2), T(2), XND, GM, RL, RK, CK,
     $               CQS, RS, GVL, KDGV, W)
      end if
C
      if(SW(3)) then
C----   COMPLEX-LOWER
        call TEMPLE (EP1N(1,3), EP2N(1,3), T(3), CK, CIJ, RS, BDI, XND,
     $               RHO, YBR, GM, GVL, KDGV, X, IX, W, IW)
      end if
C
      if(SW(4)) then
C----   CHAIN
        call FOREST (EP1N(1,4), EP2N(1,4), T(4), RHO, YBR, XND, GM, RL,
     $               CK, CIJ, RS, RK, BDI, CQTAIL, TAUK, CQS, AL, GVL,
     $               KDGV, X, IX, W, IW)
      end if
C     !END
      call BYE ('PYLON')
C
      return
      end
