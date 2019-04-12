      subroutine GROPE
     $(XNH1,ELLH,XNHE11,ELLHE,XNHEP,ELLHE2,XNE,XNP,XNHE2K,ELLE,
     $ A,B,C,D)
C
C     Rudolf Loeser, 1984 May 08
C---- Computes terms for ESS.
C     !DASH
      save
C     !DASH
      real*8 A, B, C, D, ELLE, ELLH, ELLHE, ELLHE2, FOUR, TWO, XNE,
     $       XNH1, XNHE11, XNHE2K, XNHEP, XNP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 5),FOUR  )
C     !DASH
      external HI, BYE
C
      call HI ('GROPE')
C     !BEG
      A = XNH1*ELLH
      B = TWO*XNHE11*ELLHE
      C = XNHEP*ELLHE2
      D = (XNE+XNP+XNHEP+FOUR*XNHE2K)*ELLE
C     !END
      call BYE ('GROPE')
C
      return
      end
