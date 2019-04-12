      subroutine HAWSER
     $(KKU,EP1,EP2,RK,RL,RKC,RLC,RQU,RQS,B1,PIJ,IETA,KASE,KRPIJ,U,F1,SP,
     $ D,XLF,XLP,RNDT,TAUK,ERT,S,SLY,XJIK,RKO,RLO,RP,RS,WR1,DNRT,DNRTC,
     $ PIS,TREFF,SL,EL,ZL)
C
C     Rudolf Loeser, 1975 Jan 02
C---- Prints some of the results of the Lyman calculation.
C     (This is version 2 of HAWSER.)
C     !DASH
      save
C     !DASH
      real*8 B1, D, DNRT, DNRTC, EL, EP1, EP2, ERT, EXLYM, F1, PIJ, PIS,
     $       RK, RKC, RKO, RL, RLC, RLO, RNDT, RP, RQS, RQU, RS, S, SL,
     $       SLY, SP, TAUK, TGLYM, TREFF, U, WR1, XJIK, XLF, XLP, YL,
     $       YPRE, ZL
      integer IETA, IQINC, IQLYA, KASE, KKPR, KKU, KN, KOLEV, KRPIJ,
     $        LNLIM, METEP, MO, N, NCR, NL, NSL
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(40),NSL)
      equivalence (JZQ(32),NCR)
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
      equivalence (KZQ( 32),KKPR )
      equivalence (KZQ( 16),LNLIM)
      equivalence (RZQ( 19),YL   )
      equivalence (RZQ( 20),YPRE )
      equivalence (KZQ( 33),KOLEV)
      equivalence (RZQ( 12),EXLYM)
      equivalence (RZQ( 13),TGLYM)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 8),MO   )
C     !EJECT
C---- OPTIONS     as of 2007 Jan 12
C
C     Processing and printing control switches.
C
      integer     NOOPT
      parameter   (NOOPT=345)
C     (When NOOPT is changed, FOP, FURRY, REFAULT must be recompiled!)
      integer     IQQ,IQD,IQT
      character   ONAME*8
      dimension   IQQ(NOOPT),IQD(NOOPT),IQT(NOOPT), ONAME(NOOPT)
C
      common      /OPTIONS/ IQQ
C     IQQ is the actual option status.
      common      /OPTION1/ IQD
C     IQD is the default option status.
      common      /OPTION2/ ONAME
C     ONAME is the option name (use 0000 for unused names).
      common      /OPTION3/ IQT
C     IQT is the option type:
C     1 = printout; 2 = calculation; 3 = miscellaneous; 4 = debug.
      equivalence (IQQ( 51),IQINC)
      equivalence (IQQ(255),IQLYA)
C     !DASH
      external  DRIP1, DRIP2, DRIP3, DRIP4, DRIP5, DRIP6, HI, BYE
      intrinsic min
C
C               RK(N,NL), RL(N,NL), RKC(N,NL), RLC(N,NL), EL(N), SL(N),
      dimension RK(*),    RL(*),    RKC(*),    RLC(*),    EL(*), SL(*),
C
C               RQS(N,NL), B1(N), U(N), D(N), SP(N,KKX), XLF(N), ZL(N),
     $          RQS(*),    B1(*), U(*), D(*), SP(*),     XLF(*), ZL(*),
C
C               XLP(N), RNDT(N), TAUK(N), F1(N), PIS(N,NL), SLY(N,KKX),
     $          XLP(*), RNDT(*), TAUK(*), F1(*), PIS(*),    SLY(*),
C
C               XJIK(N,KKX), RKO(N), RLO(N), S(N), TREFF(N), RQU(N,NL),
     $          XJIK(*),     RKO(*), RLO(*), S(*), TREFF(*), RQU(*),
C
C               PIJ(N,NL,NL), WR1(N), DNRT(N), DNRTC(N), EP1(N), RS(N),
     $          PIJ(*),       WR1(*), DNRT(*), DNRTC(*), EP1(*), RS(*),
C
C               EP2(N), ERT(N), RP(N)
     $          EP2(*), ERT(*), RP(*)
C     !EJECT
C
      call HI ('HAWSER')
C     !BEG
      if(MO.gt.0) then
        if(IQLYA.gt.0) then
          call DRIP5 (MO, N, NL, KOLEV, S, B1, RK, RL, KASE, IETA,
     $                LNLIM, EXLYM, TGLYM, YL)
C
          call DRIP6 (MO, N, TAUK, S, ERT, IETA, SL, EL, ZL)
        else
C
          KN = min(KKPR,KKU)
          if(KN.le.0) then
            KN = KKU
          end if
C
          call DRIP1 (N, MO, IETA, U, F1, SP, EP1, EP2, D, XLF, XLP,
     $                RNDT, IQINC, METEP, KN, NCR)
C
          call DRIP2 (N, NL, MO, IETA, TAUK, ERT, S, B1, SLY, XJIK,
     $                RQU, RQS, KASE, LNLIM, YL, YPRE, KN, KOLEV,
     $                EXLYM, TGLYM)
C
          call DRIP6 (MO, N, TAUK, S, ERT, IETA, SL, EL, ZL)
C
          call DRIP4 (MO, N, NL, RKC, RLC, DNRTC)
C
          call DRIP3 (N, NL, MO, IETA, RK, RKO, WR1, RL, RLO, RP, RS,
     $                PIJ, DNRT, NSL, PIS, KRPIJ, TREFF)
        end if
      end if
C     !END
      call BYE ('HAWSER')
C
      return
      end
