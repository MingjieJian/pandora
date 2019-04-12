      subroutine LACO
     $(TRMAX,TINT)
C
C     Rudolf Loeser, 2001 Dec 28
C---- Computes graph axis parameter, for COAL.
C     !DASH
      save
C     !DASH
      real*8 FIVE, TEN, TINT, TRMAX, TWO, XML
      integer MIOW
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 3),TWO   )
      equivalence (DLIT( 6),FIVE  )
      equivalence (DLIT(11),TEN   )
C     !DASH
      external HI, BYE
C
      call HI ('LACO')
C     !BEG
      XML  = log10(TRMAX)
      MIOW = XML
      TINT = TEN**MIOW
      if(MIOW.le.4) then
        TINT = TINT/FIVE
      else
        TINT = TINT/TWO
      end if
C     !END
      call BYE ('LACO')
C
      return
      end
