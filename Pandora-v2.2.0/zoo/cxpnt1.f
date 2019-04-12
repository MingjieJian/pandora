      subroutine CXPNT1
     $(U,XM,EMX,EI)
C     Rudolf Loeser, 1979 Jan 08
C---- Subroutine for CEXPINT. - Computes
C     EI(XM) for XM .lt. -1.0
C     Hastings polynomial approximation.
C     !DASH
      save
C     !DASH
      real*8 A1, A2, A3, A4, B1, B2, B3, B4, EI, EMX, F, P, U, XM
C     !DASH
      data A1, A2 /8.573328740D+0, 1.805901697D+1/
      data A3, A4 /8.634760893D+0, 2.677737343D-1/
      data B1, B2 /9.573322345D+0, 2.563295615D+1/
      data B3, B4 /2.109965308D+1, 3.958496923D+0/
C
C     !BEG
      F  = EMX/XM
      P  = ((((U+A1)*U+A2)*U+A3)*U+A4)/((((U+B1)*U+B2)*U+B3)*U+B4)
      EI = P*F
C     !END
C
      return
      end
