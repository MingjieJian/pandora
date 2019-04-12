      subroutine CASSIA
     $(X,IX,W,IW,RHO,YBR,RL,RK,CK,CIJ,GM,BDI,CQS,XND,CQTAIL,AL,GVL,
     $ EP1,EP2,RS,TAUK)
C
C     Rudolf Loeser, 1987 Nov 05
C---- "Lyman" EP1, EP2 calculation.
C     !DASH
      save
C     !DASH
      real*8 AL, BDI, CIJ, CK, CQS, CQTAIL, EP1, EP2, GM, GVL, RHO, RK,
     $       RL, RS, T, TAUK, W, X, XND, YBR
      integer IEP1N, IEP2N, IN, IS, IW, IX, METEP, MOX, NL
      logical SW
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 2),NL )
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
C
C---- KNTLYEP     as of 1988 Feb 10
      integer     KNTLE
      parameter   (KNTLE=4)
C     The number of alternative methods for computing Lyman EPn.
C     (Used in CASSIA, MODOC.)
C     .
C     !DASH
C     !EJECT
      external MODOC, OREGON, CYCAD, DIOMEDE, PYLON, WGIVE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
C               RL(N,NSL), RK(N,NSL), CK(N,NSL), CQS(N,NSL), GVL(N,NL),
      dimension RL(*),     RK(*),     CK(*),     CQS(*),     GVL(*),
C
C               CIJ(N,NL,NL), GM(N,NSL), BDI(N,NL), RHO(N,NT), TAUK(N),
     $          CIJ(*),       GM(*),     BDI(*),    RHO(*),    TAUK(*),
C
C               XND(N,NL), AL(NL), RS(N), CQTAIL(MQT), EP1(N), EP2(N),
     $          XND(*),    AL(*),  RS(*), CQTAIL(*),   EP1(*), EP2(*),
C
C               YBR(N,NT)
     $          YBR(*)
C
      dimension IN(2)
      equivalence
     $(IN( 1),IEP1N ),(IN( 2),IEP2N )
C
      dimension T(KNTLE), SW(KNTLE)
C
      call HI ('CASSIA')
C     !BEG
      if(NL.gt.1) then
C       (Get, and allocate, W allotment)
        call MODOC   (IN,IS,MOX,'CASSIA')
C
C----   Set method switches
        call OREGON  (SW,KNTLE,METEP)
C----   Compute Epsilons
        call PYLON   (X,IX,W,IW,RHO,YBR,RL,RK,CK,CIJ,GM,BDI,CQS,XND,
     $                CQTAIL,AL,GVL,W(IEP1N),W(IEP2N),T,SW,RS,TAUK)
C----   Select results to be used
        call CYCAD   (W(IEP1N),W(IEP2N),METEP,EP1,EP2)
C----   Print comparison of results, if needed
        call DIOMEDE (W(IEP1N),W(IEP2N),METEP,T)
C
C       (Give back W allotment)
        call WGIVE   (W,'CASSIA')
      end if
C     !END
      call BYE ('CASSIA')
C
      return
      end
