      subroutine PAYDOR
     $(GMASS,N,P)
C
C     Rudolf Loeser, 2003 Nov 03
C---- Computes P from mass.
C     !DASH
      save
C     !DASH
      real*8 CGR, CON, GMASS, P, SOLSGR
      integer N
C     !COM  or  !DASH
C---- ARGUS     as of 2007 Jan 22
      real*8    RZQ
      integer   KZQ
      character QZQ*8
      dimension RZQ(183), KZQ(226), QZQ(5)
      common    /ARGUS1/ RZQ
      common    /ARGUS2/ KZQ
      common    /ARGUS3/ QZQ
C     Collections of general parameters.
      equivalence (RZQ( 16),CGR  )
C
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 9),SOLSGR)
C     !DASH
      external MOVE1, CONMUL, HI, BYE
C
C               GMASS(N), P(N)
      dimension GMASS(*), P(*)
C
      call HI ('PAYDOR')
C     !BEG
      call MOVE1  (GMASS, N, P)
      CON = CGR*SOLSGR
      call CONMUL (CON, P, N)
C     !END
      call BYE ('PAYDOR')
C
      return
      end
