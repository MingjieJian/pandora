      subroutine RAKE
     $(N,KK,AK,XK,GK,XJIKB,RKI,NL,RKO,ORK1,CP,WR1,RKW,XINCH,WMN,WMX,
     $ SMP,KOLEV,DNRT,NCR,XJIKA,RKA,RKB,RCHECK,WEIT,IMG,FO)
C
C     Rudolf Loeser, 1974 Dec 26
C---- Computes RK for ROPE.
C     !DASH
      save
C     !DASH
      real*8 AK, CP, DNRT, FO, GK, ORK1, RCHECK, RKA, RKB, RKI, RKO,
     $       RKW, SMP, WEIT, WMN, WMX, WR1, XINCH, XJIKA, XJIKB, XK,
     $       ZERO
      integer IMG, KK, KMSS, KOLEV, KWSS, N, NCR, NERM, NL
      logical lummy
      character LABEL*10
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
C     !DASH
C     !EJECT
      external KERMESS, MOVE1, FAKE, EDITH, CAPRA, KIESEL, VALDRAD,
     $         WAKE, HI, BYE
C
C               RKI(N,NL), RKO(N), CP(NL), AK(KKX), GK(KKX), RCHECK(N),
      dimension RKI(N,*),  RKO(*), CP(*),  AK(*),   GK(*),   RCHECK(*),
C
C               XJIKB(N,KKX), RKW(N), RKA(N), XK(KKX), WR1(N), DNRT(N),
     $          XJIKB(*),     RKW(*), RKA(*), XK(*),   WR1(*), DNRT(*),
C
C               WEIT(N), ORK1(N), IMG(N), RKB(N), FO(N), XJIKA(N,KKX)
     $          WEIT(*), ORK1(*), IMG(*), RKB(*), FO(*), XJIKA(*)
C
      call HI ('RAKE')
C     !BEG
      call KERMESS ('MO', KMSS)
      call CAPRA   (      KWSS)
      write (LABEL,100) KOLEV
  100 format('Lyman-RK',I2)
C
C---- Save previous RK
      call MOVE1   (RKI(1,KOLEV), N, RKO)
C---- Compute new RK
      call FAKE    (CP, RKI(1,KOLEV), KK, AK, GK, XK, XJIKB, N, KOLEV,
     $              DNRT, NCR, RCHECK, XJIKA, RKA, RKB)
C---- Edit out any negatives
      call EDITH   (RKI(1,KOLEV), N, ZERO, 1, 1, KMSS, LABEL, IMG, FO,
     $              KERMED(8), NERM, lummy)
C---- Weight with previous values
      call WAKE    (N, WR1, RKW, WEIT, XINCH, WMN, WMX, SMP, KWSS,
     $              ORK1, RKO, RKI(1,KOLEV), LABEL)
C---- Save old RK for weight adjustment in next iteration
      call MOVE1   (RKO, N, ORK1)
C---- Get rid of surviving negatives
      call KIESEL  (RKI(1,KOLEV), N, ZERO, 2, RKO, KMSS, LABEL, IMG,
     $              FO, KERMED(8), NERM, lummy)
C---- Smooth RK
      call VALDRAD (RKI(1,KOLEV), N, KMSS, LABEL)
C     !END
      call BYE ('RAKE')
C
      return
      end
