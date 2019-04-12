      subroutine GAMUA
     $(N,HE1K,HE2,HE2K,HEELE)
C
C     Rudolf Loeser, 2001 Dec 14
C---- Computes Helium electrons.
C     (This is version 2 of GAMUA.)
C     !DASH
      save
C     !DASH
      real*8 HALF, HE1K, HE2, HE2K, HEELE, TWO
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external HI, BYE
C
C               HE1K(N), HE2(N), HE2K(N), HEELE(N)
      dimension HE1K(*), HE2(*), HE2K(*), HEELE(*)
C
      call HI ('GAMUA')
C     !BEG
      do 100 I = 1,N
        HEELE(I) = HALF*(HE1K(I)+HE2(I))+TWO*HE2K(I)
  100 continue
C     !END
      call BYE ('GAMUA')
C
      return
      end
