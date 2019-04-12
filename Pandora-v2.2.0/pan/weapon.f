      subroutine WEAPON
     $(N,WEP,EP1,EP2,BDI,TAUK,OEP1,OEP2,EPS,IMG,ENW,WEIT,FO)
C
C     Rudolf Loeser, 1981 Dec 11
C---- Edits and weights Lyman Epsilons.
C     (This is version 2 of WEAPON.)
C     !DASH
      save
C     !DASH
      real*8 BDI, ENW, EP1, EP2, EPS, FO, OEP1, OEP2, TAUK, WEIT, WEP,
     $       ZERO
      integer IMG, IQEN2, IQENL, KMSS, MO, N, NERM
      logical lummy
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
      equivalence (KZQ( 95),NERM )
C
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
      equivalence (DLIT( 1),ZERO  )
C
C---- BURNET      as of 1995 Sep 08
      integer     NURBET,KERMED
      parameter   (NURBET=12)
      dimension   KERMED(NURBET)
      common      /BURNET/ KERMED
C     Counts of error messages from EDITH, for various contexts:
C      1 - "optical depth"          2 - basic b-ratios
C      3 - PRD QSF                  4 - whole-profile S
C      5 - line source function     6 - J-bar
C      7 - "Lyman" EP1              8 - "Lyman" RK
C      9 - b-values                10 - net radiative bracket - "rho"
C     11 - GTN or TAU-integrand    12 - S-from-N
C     .
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
      equivalence (IQQ( 71),IQENL)
      equivalence (IQQ( 72),IQEN2)
C     !DASH
      external KERMESS, DORSET, EDITH, HEATHER, FENN, CART, HI, BYE
C
C               EP1(N), OEP2(N), TAUK(N), OEP1(N), BDI(N,NL), WEIT(N),
      dimension EP1(*), OEP2(*), TAUK(*), OEP1(*), BDI(*),    WEIT(*),
C
C               EP2(N), EPS(N), IMG(N), ENW(N), FO(N)
     $          EP2(*), EPS(*), IMG(*), ENW(*), FO(*)
C
      call HI ('WEAPON')
C     !BEG
      call KERMESS   ('MO',KMSS)
      call DORSET    (MO)
      if(IQENL.gt.0) then
C----   Simple EP1 editing
        call EDITH   (EP1,N,ZERO,1,1,KMSS,'EP1',IMG,FO,KERMED(7),NERM,
     $                lummy)
      end if
      if(IQEN2.gt.0) then
C----   Revised EP1 editing
        call HEATHER (EP1,EP2,BDI,N)
      end if
C---- Newest EP1 editing procedure
      call FENN      (TAUK,EP1,EPS,N)
C---- EP1 and EP2 weighting
      call CART      (EP1,OEP1,EP2,OEP2,WEP,N,KMSS,ENW,WEIT)
C     !END
      call BYE ('WEAPON')
C
      return
      end
