      subroutine LINTEL
     $(EP1,EP2,RKW,CQTAIL,XNU,XK,YLYM,GK,NO)
C
C     Rudolf Loeser, 1981 Jan 02
C---- Printing routine for DABBLE, dealing with Lyman data.
C     (This is version 2 of LINTEL.)
C     !DASH
      save
C     !DASH
      real*8 CQTAIL, EP1, EP2, EPCBR, EXLYM, GK, OPF, RKW, TGLYM, WEP,
     $       WSM, XK, XLMA, XLMB, XLME, XLMF, XLMR, XLMT, XNU, XNUK, YL,
     $       YLYM
      integer IDNRT, INFSM, INLSM, IQEPS, IQINC, IQLYM, KK, KOLEV,
     $        KXLYM, LNLIM, LYMIT, METEP, MQT, N, NCR, NL, NO
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
      equivalence (JZQ( 1),N  )
      equivalence (JZQ( 2),NL )
      equivalence (JZQ(10),KK )
      equivalence (JZQ(39),MQT)
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
      equivalence (RZQ(  9),XNUK )
      equivalence (RZQ( 62),EPCBR)
      equivalence (RZQ( 40),WEP  )
      equivalence (RZQ( 41),OPF  )
      equivalence (RZQ( 12),EXLYM)
      equivalence (RZQ( 13),TGLYM)
      equivalence (RZQ( 19),YL   )
      equivalence (RZQ( 26),XLMT )
      equivalence (RZQ( 33),XLMF )
      equivalence (RZQ( 27),XLME )
      equivalence (RZQ( 35),XLMA )
      equivalence (RZQ( 36),XLMB )
      equivalence (RZQ( 37),XLMR )
      equivalence (RZQ( 34),WSM  )
      equivalence (KZQ( 23),INFSM)
      equivalence (KZQ( 24),INLSM)
      equivalence (KZQ(148),IDNRT)
      equivalence (KZQ( 33),KOLEV)
      equivalence (KZQ( 25),METEP)
      equivalence (KZQ( 19),LYMIT)
      equivalence (KZQ( 16),LNLIM)
      equivalence (KZQ(216),KXLYM)
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
      equivalence (IQQ(122),IQEPS)
      equivalence (IQQ( 51),IQINC)
      equivalence (IQQ( 13),IQLYM)
C     !DASH
      external PADMA, CUT, CHEAP, CRUX, HARDUN, TEGU, KOMODO, HI, BYE
C
C               EP1(N), EP2(N), RKW(N), CQTAIL(MQT), GK(KK), YLYM(KK),
      dimension EP1(*), EP2(*), RKW(*), CQTAIL(*),   GK(*),  YLYM(*),
C
C               XNU(NSL), XK(KK)
     $          XNU(*),   XK(*)
C
      call HI ('LINTEL')
C     !BEG
      if((IQLYM.gt.0).and.(NO.gt.0)) then
C----   Heading
        call PADMA  (NO, '"LYMAN" Calculation')
        call CHEAP  (NO)
C----   XK, GK, and RK1 weights
        call CUT    (RKW, KOLEV, N, NL, XNUK, XNU, XK, YLYM, GK, KK,
     $               KXLYM, NO)
C----   Lyman-epsilons data
        call CRUX   (EP1, EP2, METEP, EPCBR, WEP, IQEPS, CQTAIL, N,
     $               MQT, NO)
C----   Incident Radiation stuff
        call HARDUN (OPF, NCR, IQINC, IDNRT, NO)
C----   Source function parameters
        call TEGU   (LYMIT, LNLIM, EXLYM, TGLYM, YL, NO)
C----   Editing parameters
        call KOMODO (KOLEV, XLMT, XLMF, XLME, XLMA, XLMB, XLMR, INFSM,
     $               INLSM, WSM, N, NO)
      end if
C     !END
      call BYE ('LINTEL')
C
      return
      end
