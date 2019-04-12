      subroutine GUSH
     $(N,HND,HELABD,GD)
C
C     Rudolf Loeser, 1979 Dec 20
C---- Computes gas density.
C     !DASH
      save
C     !DASH
      real*8 GD, HELABD, HEMASS, HND, HYMASS, ONE
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
      equivalence (PCON( 8),HYMASS)
      equivalence (PCON( 5),HEMASS)
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
C               HND(N), GD(N), HELABD(N)
      dimension HND(*), GD(*), HELABD(*)
C
      call HI ('GUSH')
C     !BEG
      do 100 I = 1,N
        GD(I) = HND(I)*(HYMASS*(ONE+HEMASS*HELABD(I)))
  100 continue
C     !END
      call BYE ('GUSH')
C
      return
      end
