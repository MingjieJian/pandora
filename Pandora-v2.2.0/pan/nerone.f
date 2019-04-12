      subroutine NERONE
     $(IU,IL,PMSK,CSK)
C     Rudolf Loeser, 1991 May 13
C---- Computes default CSK for Hydrogen.
C
C     From:  Sutton, K. 1978, JQSRT, 20, 233.
C     !DASH
      save
C     !DASH
      real*8 A1, AUL, CSK, ELL2, FAC, ONE, PMSK, R, YOO2
      integer IL, IU
C     !COM
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
      data A1,FAC /6.42D-1, 7.06D-6/
C
      call HI ('NERONE')
C     !BEG
      if((IU-IL).eq.1) then
        AUL = A1
      else
        AUL = ONE
      end if
C
      ELL2 = IL**2
      YOO2 = IU**2
      R = ((ELL2*YOO2)**2)/(YOO2-ELL2)
C
      CSK = (AUL*FAC*R)*PMSK
C     !END
      call BYE ('NERONE')
C
      return
      end
