      subroutine CXPNT3
     $(XM,EMX,EI)
C     Rudolf Loeser, 1979 Jan 09
C---- Subroutine for CEXPINT. - Computes
C     EI(XM) for 5.0 .le. XM .lt. 24.5
C     Table lookup and interpolation.
C     !DASH
      save
C     !DASH
      real*8 CRIT, DELTA, DELTAX, EI, EMX, HALF, POWER, TAB, TINT, XM,
     $       XZERO, Y, ZERO
      integer I
C     !DASH
      intrinsic abs
C
      dimension TAB(20),TINT(7)
C
      data TAB /
     $ 2.707662555D-1, 2.131473101D-1, 1.746297218D-1, 1.477309984D-1,
     $ 1.280843565D-1, 1.131470205D-1, 1.014028126D-1, 9.191454540D-2,
     $ 8.407902920D-2, 7.749225150D-2, 7.187354050D-2, 6.702156100D-2,
     $ 6.278786420D-2, 5.906040440D-2, 5.575290770D-2, 5.279779530D-2,
     $ 5.014133860D-2, 4.774026000D-2, 4.555929450D-2, 4.356940880D-2/
      data TINT /1.D0, 2.D0, 3.D0, 4.D0, 5.D0, 6.D0, 7.D0/
      data CRIT /1.D-8/
      data ZERO, HALF /0.D0, 5.D-1/
C
C     !BEG
      I = XM+HALF
      XZERO = I
      DELTA = XZERO-XM
      EI = TAB(I-4)
      if(DELTA.ne.ZERO) then
        Y = EI
        DELTAX = DELTA/XZERO
        POWER  = TINT(1)/DELTAX
        do 100 I = 1,7
          POWER = POWER*DELTAX
          Y  = ((Y-POWER/XZERO)*DELTA)/TINT(I)
          EI = EI+Y
          if(abs(Y/EI).lt.CRIT) goto 101
  100   continue
  101   continue
      end if
      EI = EMX*EI
C     !END
C
      return
      end
