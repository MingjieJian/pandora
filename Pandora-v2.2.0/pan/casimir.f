      subroutine CASIMIR
     $(N,XNE,HE2K,XNC)
C
C     Rudolf Loeser, 1994 May 04
C---- Computes total charged particle density, XNC.
C     !DASH
      save
C     !DASH
      real*8 HE2K, TWO, XNC, XNE
      integer N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external ARRSUB, CONMUL, MOVE1, HI, BYE
C
C               XNE(N), HE2K(N), XNC(N)
      dimension XNE(*), HE2K(*), XNC(*)
C
      call HI ('CASIMIR')
C     !BEG
      call MOVE1  (XNE,N,XNC)
      call CONMUL (TWO,XNC,N)
      call ARRSUB (XNC,HE2K,XNC,N)
C     !END
      call BYE ('CASIMIR')
C
      return
      end
