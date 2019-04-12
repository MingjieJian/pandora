      subroutine JINGU
     $(L,B)
C
C     Rudolf Loeser, 1990 Dec 07
C---- Computes b, for the Vriens & Smeets model of the Hydrogen atom.
C     !DASH
      save
C     !DASH
      real*8 B, C1, C2, C3, C4, C5, ELL, ELLOG, S
      integer L
C     !DASH
      external HI, BYE
C
      data C1,C2,C3,C4,C5 /1.4D0, -7.D-1, -5.1D-1, 1.16D0, -5.5D-1/
C
      call HI ('JINGU')
C     !BEG
      ELL   = L
      S     = (C2+(C3+(C4+C5/ELL)/ELL)/ELL)
      ELLOG = log(ELL)
C
      B = ((C1*ELLOG)+S)/ELL
C     !END
      call BYE ('JINGU')
C
      return
      end
