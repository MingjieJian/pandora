      subroutine PACA
     $(VEC,X,F)
C
C     Rudolf Loeser, 1986 Jul 30
C---- Computes values of the integrand F and the intermediate function H
C     for the "Sobolev" escape probability calculation.
C     !DASH
      save
C     !DASH
      real*8 D, F, H, ONE, S, VEC, X, dummy
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, QEXP4, HI, BYE
C
      dimension VEC(4)
C
      call HI ('PACA')
C     !BEG
      S = X**2
      D = S*VEC(2)+(ONE-S)*VEC(3)
      call DIVIDE (VEC(1),D,H)
      VEC(4) = H
      call QEXP4  (H,dummy,2,F)
C     !END
      call BYE ('PACA')
C
      return
      end
