      subroutine HOLDIN
     $(N,PTO,PMG,GMASS)
C
C     Rudolf Loeser, 1983 May 25
C---- Computes mass of a column of gas.
C     !DASH
      save
C     !DASH
      real*8 C1, CGR, GMASS, ONE, PMG, PTO, SOLSGR
      integer N
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
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external MOVE1, CONSUB, CONMUL, HI, BYE
C
C               PTO(N), PMG(N), GMASS(N)
      dimension PTO(*), PMG(*), GMASS(*)
C
      call HI ('HOLDIN')
C     !BEG
      C1 = ONE/(CGR*SOLSGR)
C
      call MOVE1  (PTO, N, GMASS)
      call CONSUB (PMG(1), GMASS, N)
      call CONMUL (C1,     GMASS, N)
C     !END
      call BYE ('HOLDIN')
C
      return
      end
