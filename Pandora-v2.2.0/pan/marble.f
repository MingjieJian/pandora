      subroutine MARBLE
     $(X,W,HND,HELABD,DD,R,P,IMG)
C
C     Rudolf Loeser, 1980 Nov 07
C---- Computes R and P, for H.S.E.
C     (This is version 7 of MARBLE.)
C     !DASH
      save
C     !DASH
      real*8 CGR, CON36, DD, FAC, HELABD, HEMASS, HND, ONE, P, PZERO, R,
     $       W, X
      integer I, IMG, N, jummy
      logical lummy1, lummy2
      character LABEL*100
C     !COM
C---- COUNTS      as of 2006 May 09
      integer     JZQ
      dimension   JZQ(58)
      common      /COUNTS/ JZQ
C     Tables lengths.
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
      equivalence (RZQ( 72),PZERO)
      equivalence (RZQ( 16),CGR  )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 5),HEMASS)
C     !DASH
C     !EJECT
      external RIGEL, DISMAL, CONADD, HI, BYE
C
      dimension X(*), W(*)
C
C               HND(N), R(N), P(N), IMG(N), HELABD(N), DD(N)
      dimension HND(*), R(*), P(*), IMG(*), HELABD(*), DD(*)
C
      data LABEL /'P, for H.S.E.'/
C
      call HI ('MARBLE')
C     !BEG
C---- Compute integrand
      call RIGEL  (36, CON36)
      FAC = CGR*CON36
      do 100 I = 1,N
        R(I) = (FAC*HND(I))*(ONE+HEMASS*HELABD(I))-DD(I)
  100 continue
C
C---- Compute integral
C     (Note: DISMAL determines that this is an integral over TAUK
C     and therefore divides the integrand by PREF.)
      call DISMAL (X, W, 1, N, R, P, LABEL, jummy, lummy1, lummy2,
     $             IMG)
C
C---- Add PZERO to all integrated values, getting fianl P
      call CONADD (PZERO, P, N)
C     !END
      call BYE ('MARBLE')
C
      return
      end
