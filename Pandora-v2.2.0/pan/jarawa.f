      subroutine JARAWA
     $(N,NL,BDI,IMG,FO)
C
C     Rudolf Loeser, 1987 Dec 17
C---- Edits b values, for WILY.
C     !DASH
      save
C     !DASH
      real*8 BDI, FO, ZERO
      integer IMG, J, N, NERM, NL
      logical lummy
      character LABEL*40
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
      external  EDITH, HI, BYE
C
C               BDI(N,NL), IMG(N), FO(N)
      dimension BDI(N,*),  IMG(*), FO(*)
C     !EJECT
C
      call HI ('JARAWA')
C     !BEG
      do 101 J = 1,NL
        write (LABEL,100) J
  100   format('Departure coefficients, b, for level ',I3)
C
        call EDITH (BDI(1,J),N,ZERO,2,2,1,LABEL,IMG,FO,KERMED(9),NERM,
     $              lummy)
  101 continue
C     !END
      call BYE ('JARAWA')
C
      return
      end
