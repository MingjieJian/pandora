      subroutine GASP
     $(TE,HND,HELABD,H2N,PEL,PGS,N)
C
C     Rudolf Loeser, 1983 Jul 05
C---- Computes gas pressure.
C     !DASH
      save
C     !DASH
      real*8 BOLZMN, H2N, HELABD, HND, ONE, PEL, PGS, TE
      integer I, N
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (PCON( 2),BOLZMN)
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
C               HELABD(N), HND(N), H2N(N), PEL(N), PGS(N), TE(N)
      dimension HELABD(*), HND(*), H2N(*), PEL(*), PGS(*), TE(*)
C
      call HI ('GASP')
C     !BEG
      do 100 I = 1,N
        PGS(I) = (BOLZMN*TE(I))*((ONE+HELABD(I))*HND(I)-H2N(I))+PEL(I)
  100 continue
C     !END
      call BYE ('GASP')
C
      return
      end
