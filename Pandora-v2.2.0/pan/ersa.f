      subroutine ERSA
     $(AMASS,TE,W,Y,H)
C
C     Rudolf Loeser, 1991 May 15
C---- Computes intermediates for DERVENI.
C     !DASH
      save
C     !DASH
      real*8 AMASS, EMY, FH, FY, H, ONE, RAT, TE, TOY, TWO, W, Y
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 3),TWO   )
C     !DASH
      external DIVIDE, HI, BYE
C
      data FY,FH /1.578D5, 1.21D-6/
C
      call HI ('ERSA')
C     !BEG
      Y   = (FY/TE)*W
      EMY = exp(-Y)
C
      call DIVIDE (TWO, Y, TOY)
      call DIVIDE (FH, ((Y*AMASS)**2), RAT)
C
      H = ((ONE+TOY)*EMY)/(ONE+RAT)
C     !END
      call BYE ('ERSA')
C
      return
      end
