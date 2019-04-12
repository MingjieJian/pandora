      subroutine CXPNT4
     $(X,EMX,EI)
C
C     Rudolf Loeser, 1979 Jan 09
C---- Subroutine for CEXPINT. - Computes
C     EI(XM) for 24.5 .le. XM
C     !DASH
      save
C     !DASH
      real*8 A1, A2, A3, B1, B2, B3, B4, EI, EMX, X
C     !DASH
      data A1, A2, A3     /1.5D+1, 5.8D+1, 5.0D+1/
      data B1, B2, B3, B4 /1.6D+1, 7.2D+1, 9.6D+1, 2.4D+1/
C
C     !BEG
      EI = ((((X-A1)*X+A2)*X-A3)*EMX)/((((X-B1)*X+B2)*X-B3)*X+B4)
C     !END
C
      return
      end
