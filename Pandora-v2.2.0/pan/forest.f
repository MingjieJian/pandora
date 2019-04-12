      subroutine FOREST
     $(EP1,EP2,T,RHO,YBR,XND,GM,RL,CK,CIJ,RS,RK,BDI,CQTAIL,TAUK,CQSI,
     $ AL,GVL,KDGV,X,IX,W,IW)
C
C     Rudolf Loeser, 1981 Dec 14
C---- Computes Lyman epsilons by a "CHAIN-like" method.
C     !DASH
      save
C     !DASH
      real*8 AL, BDI, CIJ, CK, CQSI, CQTAIL, EP1, EP2, EPCBR, GM, GVL,
     $       RHO, RK, RL, RS, T, TAUK, TIN, TUT, W, X, XLMT, XND, YBR
      integer IARO, IC1LT, ICL1T, IN, IP1L, IPKL, IPL1, IPLK, IPS1, IQI,
     $        IS, ISPKL, IW, IX, KDGV, KOLEV, MOX, MQT, N, NL, NSL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(39),MQT)
      equivalence (JZQ(40),NSL)
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
      equivalence (KZQ( 33),KOLEV)
      equivalence (RZQ( 26),XLMT )
      equivalence (RZQ( 62),EPCBR)
C     !DASH
      external MACADAM, COCOS, SECOND, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               TAUK(N), GM(N,NSL), RHO(N,NT), XND(N,NL), CIJ(N,NL,NL),
      dimension TAUK(*), GM(*),     RHO(*),    XND(*),    CIJ(*),
C
C               CQSI(N,NSL), CK(N,NSL), GVL(N,NL), EP2(N), CQTAIL(MQT),
     $          CQSI(*),     CK(*),     GVL(*),    EP2(*), CQTAIL(*),
C
C               RK(N,NSL), BDI(N,NL), EP1(N), YBR(N,NT), RL(N,NSL),
     $          RK(*),     BDI(*),    EP1(*), YBR(*),    RL(*),
C
C               AL(NL), RS(N)
     $          AL(*),  RS(*)
C
      dimension IN(10)
      equivalence
     $(IN( 1),IARO  ),(IN( 2),ISPKL ),(IN( 3),ICL1T ),(IN( 4),IQI   ),
     $(IN( 5),IC1LT ),(IN( 6),IPS1  ),(IN( 7),IPKL  ),(IN( 8),IPL1  ),
     $(IN( 9),IPLK  ),(IN(10),IP1L  )
C     !EJECT
C
      call HI ('FOREST')
C     !BEG
      call SECOND  (TIN)
C     (Get, and allocate, W allotment)
      call MACADAM (IN, IS, MOX, 'FOREST')
C
      call COCOS   (N, NL, KOLEV, RHO, YBR, XND, GM, RL, CK, RS, EP1,
     $              EP2, W(IARO), W(ICL1T), W(IC1LT), CIJ, W(IPKL),
     $              W(IPL1), W(IPLK), W(IP1L), RK, W(IQI), BDI, MQT,
     $              CQTAIL, TAUK, XLMT, CQSI, NSL, W(ISPKL), EPCBR,
     $              W(IPS1), AL, GVL, KDGV, X, IX)
C
C     (Give back W allotment)
      call WGIVE   (W, 'FOREST')
C
      call SECOND  (TUT)
      T = TUT-TIN
C     !END
      call BYE ('FOREST')
C
      return
      end
