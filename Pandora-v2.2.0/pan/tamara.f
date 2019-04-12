      subroutine TAMARA
     $(XNE,W1,W2,CPR,CHI,ETA)
C
C     Rudolf Loeser, 1978 Aug 11
C---- Computes degree of ionization, in LTE.
C     (This is version 2 of TAMARA.)
C     !DASH
      save
C     !DASH
      real*8 CHI, CPR, ETA, EX, FL, ONE, W1, W2, WX, XNE
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, HI, BYE
C
      call HI ('TAMARA')
C     !BEG
      EX = exp(W2*CHI)
      call DIVIDE (W1, XNE, WX)
C
      FL = WX*CPR*EX
      call DIVIDE (FL, (ONE+FL), ETA)
C     !END
      call BYE ('TAMARA')
C
      return
      end
