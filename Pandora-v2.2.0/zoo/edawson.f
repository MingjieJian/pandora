      subroutine EDAWSON
     $(X,Y,VAL)
C     Rudolf Loeser, 1990 Dec 05
C---- Part of DRAYSON: computes Voigt function in Region I
C     !DASH
      save
C     !DASH
      real*4 CON, D0, D1, D2, D3, D4, DX, FAC, FIVE, H, HN, OFF, ONE,
     $       RI, ROOTPI, TWO, U, UU, V, VAL, VV, X, X2, Y, Y2
      integer I, M, MX, N
      logical KILROY
C     !COM
      common /CDAWSON/ H,HN,RI,D0,D1,D2,D3,D4,KILROY
C     !DASH
      external TDAWSON
C
      dimension HN(25),RI(15),D0(25),D1(25),D2(25),D3(25),D4(25)
C
      data H /2.01E-1 /
      data RI / -5.E-1, -1.E0, -1.5E0, -2.E0, -2.5E0, -3.E0,
     $          -3.5E0, -4.E0, -4.5E0, -5.E0, -5.5E0, -6.E0,
     $          -6.5E0, -7.E0, -7.5E0 /
      data KILROY / .true. /
C
      data ROOTPI / 1.7724539E0 /
      data OFF, FAC / 1.25E1, 8.E-1 /
      data ONE, TWO, FIVE / 1.E0, 2.E0, 5.E0 /
C     !EJECT
C
C     !BEG
      if(KILROY) then
C----   Tabulate Dawson's function at mesh points
        call TDAWSON
        KILROY = .false.
      end if
C
      Y2  = Y**2
      X2  = X**2
      CON = TWO/ROOTPI
C---- Compute Dawson's function at X from Taylor series
      N  = X/H
      M  = N+1
      DX = X-HN(M)
      U  = (((D4(M)*DX+D3(M))*DX+D2(M))*DX+D1(M))*DX+D0(M)
      V  = ONE-TWO*X*U
C---- Taylor series expansion about Y = 0
      VV = (exp(Y2-X2)*cos(TWO*X*Y))/CON-Y*V
      UU = -Y
      MX = FIVE+(OFF-X)*Y*FAC
      do 100 I = 2,MX,2
        U  =  (X*V+U)/RI(I  )
        V  =  (X*U+V)/RI(I+1)
        UU = -UU*Y2
        VV =   VV+V*UU
  100 continue
C
      VAL = CON*VV
C     !END
C
      return
      end
