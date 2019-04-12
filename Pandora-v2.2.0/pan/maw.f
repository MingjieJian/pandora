      subroutine MAW
     $(A,M,AN)
C
C     Rudolf Loeser, 1997 Oct 21
C---- Computes  a[new] for 4-diagonal solution.
C     (This is version 3 of MAW.)
C     !DASH
      save
C     !DASH
      real*8 A, AN, HALF
      integer I, M
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
C     !DASH
      external HI, BYE
C
C               A(N), AN(N)
      dimension A(*), AN(*)
C
      call HI ('MAW')
C     !BEG
      AN(1) = A(1)
      do 100 I = 2,M
        AN(I) = HALF*(A(I-1)+A(I))
  100 continue
C     !END
      call BYE ('MAW')
C
      return
      end
