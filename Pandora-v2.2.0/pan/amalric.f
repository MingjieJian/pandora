      subroutine AMALRIC
     $(SRAW,B,SFIN,N,IU,IL,SOB,XVAL,W,IW)
C     Rudolf Loeser, 1994 Oct 04
C---- Obtains final S-from-number density, by smoothing.
C     !DASH
      save
C     !DASH
      real*8 B, SFIN, SOB, SRAW, W, XVAL
      integer IL, INDX, IU, IW, N, jummy
      logical lummy
      character LABEL*100, TYPE*3
C     !DASH
      external  ARRDIV, SMOOTH, INDARRD, ARRMUL, HI, BYE
C
      dimension W(*), IW(*)
C
C               SRAW(N), B(N), SFIN(N), SOB(N), XVAL(N)
      dimension SRAW(*), B(*), SFIN(*), SOB(*), XVAL(*)
C
      data INDX /0/
C
      call HI ('AMALRIC')
C     !BEG
      write (LABEL,100) IU,IL
  100 format('( S-from-ND ) / B, transition ',I2,'/',I2)
C
      call INDARRD (XVAL, 1, 1, N)
C---- Form ratio
      call ARRDIV  (SRAW, B, SOB, N)
C---- First, linear smoothing, ...
      TYPE = 'lin'
      call SMOOTH  (XVAL, SOB, N, TYPE, LABEL, INDX, W, IW,
     $              jummy, lummy)
C---- ... then, logarithmic smoothing
      TYPE = 'log'
      call SMOOTH  (XVAL, SOB, N, TYPE, LABEL, INDX, W, IW,
     $              jummy, lummy)
C---- Recover final S
      call ARRMUL  (SOB, B, SFIN, N)
C     !END
      call BYE ('AMALRIC')
C
      return
      end
