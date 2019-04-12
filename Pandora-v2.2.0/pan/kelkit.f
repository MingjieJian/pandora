      subroutine KELKIT
     $(IU,IL,CVW,XLM)
C
C     Rudolf Loeser, 1990 Oct 09
C---- Computes the default value of Hydrogen CVW, the van der Waals
C     line broadening parameter.
C     !DASH
      save
C     !DASH
      real*8 BRAK, CVW, DIV, ELL2, FAC, ONE, TERM, TWO, WAVE, XLM, XPN,
     $       YOO2
      integer IL, IU
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
      external HI, BYE
C
      data FAC,XPN,DIV /2.992D-5, 4.D-1, 5.D3/
C
      call HI ('KELKIT')
C     !BEG
      YOO2 = IU**2
      ELL2 = IL**2
      BRAK = YOO2*(TWO*YOO2+ONE)-ELL2*(TWO*ELL2+ONE)
      TERM = BRAK**XPN
      WAVE = (XLM/DIV)**2
C
      CVW = FAC*WAVE*TERM
C     !END
      call BYE ('KELKIT')
C
      return
      end
