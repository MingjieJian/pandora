      subroutine ROMATIN
     $(TE,CI1)
C
C     Rudolf Loeser, 2005 Aug 12
C---- Computes CI1, the collisional ionization coefficient for
C     level 1 of Hydrogen, for a given value of temperature, TE.
C     Based on measurements of
C
C     M. B. Shah, D, S, Elliott & H. B. Gilbody 1987,
C     J.Phys.B, 20, 3501.
C
C     (This is version 2 of ROMATIN.)
C     !DASH
      save
C     !DASH
      real*8 CI1, CON24, CON72, D, E, EX, F, FAC, FP, HALF, PW, S, TE,
     $       TF, TP, XM, Y, ZERO
      integer I, N
      logical KILROY
C     !DASH
      external RIGEL, HI, BYE
C
      parameter (N=117)
      dimension Y(N), S(N), D(N)
C
      data (Y(I),I= 1, 60)/
     $ 13.6D0, 13.7D0, 13.8D0, 13.9D0, 14.0D0,
     $ 14.1D0, 14.2D0, 14.3D0, 14.4D0, 14.5D0,
     $ 14.6D0, 14.8D0, 15.0D0, 15.1D0, 15.2D0,
     $ 15.4D0, 15.6D0, 15.9D0, 16.1D0, 16.4D0,
     $ 16.6D0, 16.9D0, 17.1D0, 17.4D0, 17.6D0,
     $ 17.9D0, 18.1D0, 18.4D0, 18.7D0, 19.0D0,
     $ 19.3D0, 19.6D0, 20.0D0, 20.4D0, 20.9D0,
     $ 21.4D0, 22.0D0, 22.6D0, 23.3D0, 24.0D0,
     $ 24.8D0, 25.6D0, 26.6D0, 27.3D0, 28.3D0,
     $ 29.3D0, 30.5D0, 31.6D0, 32.8D0, 34.1D0,
     $ 35.4D0, 36.7D0, 38.1D0, 39.6D0, 41.2D0,
     $ 42.9D0, 44.7D0, 46.6D0, 48.6D0, 50.7D0/
      data (Y(I),I=61,117)/
     $ 52.9D0, 55.2D0, 57.6D0, 60.1D0, 63.0D0,
     $ 66.0D0, 69.0D0, 72.1D0, 75.5D0, 79.5D0,
     $ 84.0D0, 89.0D0, 94.0D0, 102.0D0, 103.0D0,
     $ 113.0D0, 121.0D0, 130.2D0, 138.2D0, 148.2D0,
     $ 158.2D0, 168.2D0, 178.2D0, 188.2D0, 198.2D0,
     $ 213.2D0, 228.2D0, 248.2D0, 268.2D0, 288.0D0,
     $ 317.9D0, 347.9D0, 387.9D0, 427.9D0, 467.9D0,
     $ 508.2D0, 548.2D0, 598.2D0, 668.2D0, 748.2D0,
     $ 818.2D0, 898.2D0, 998.2D0, 1100.0D0, 1200.0D0,
     $ 1300.0D0, 1506.7D0, 1662.7D0, 1848.1D0, 1998.1D0,
     $ 2198.1D0, 2448.1D0, 2698.1D0, 2998.1D0, 3298.1D0,
     $ 3548.1D0, 3998.1D0/
C     !EJECT
      data (S(I),I= 1, 60)/
     $ 0.D0, 0.0412D0, 0.0892D0, 0.141D0, 0.194D0,
     $ 0.250D0, 0.306D0, 0.364D0, 0.423D0, 0.483D0,
     $ 0.544D0, 0.661D0, 0.762D0, 0.820D0, 0.870D0,
     $ 0.990D0, 1.08D0, 1.25D0, 1.37D0, 1.45D0,
     $ 1.63D0, 1.68D0, 1.73D0, 1.96D0, 2.07D0,
     $ 2.15D0, 2.22D0, 2.35D0, 2.50D0, 2.61D0,
     $ 2.75D0, 2.81D0, 2.93D0, 3.11D0, 3.34D0,
     $ 3.39D0, 3.61D0, 3.76D0, 4.01D0, 4.15D0,
     $ 4.30D0, 4.44D0, 4.57D0, 4.75D0, 4.95D0,
     $ 5.01D0, 5.10D0, 5.27D0, 5.39D0, 5.53D0,
     $ 5.59D0, 5.74D0, 5.83D0, 5.78D0, 5.89D0,
     $ 6.02D0, 6.07D0, 6.08D0, 6.23D0, 6.27D0/
      data (S(I),I=61,117)/
     $ 6.19D0, 6.23D0, 6.21D0, 6.13D0, 6.14D0,
     $ 6.11D0, 6.11D0, 6.01D0, 5.96D0, 5.91D0,
     $ 5.84D0, 5.78D0, 5.59D0, 5.40D0, 5.42D0,
     $ 5.23D0, 5.07D0, 5.05D0, 4.83D0, 4.62D0,
     $ 4.55D0, 4.43D0, 4.28D0, 4.10D0, 3.98D0,
     $ 3.79D0, 3.61D0, 3.43D0, 3.31D0, 3.03D0,
     $ 2.84D0, 2.66D0, 2.50D0, 2.31D0, 2.15D0,
     $ 2.00D0, 1.86D0, 1.77D0, 1.59D0, 1.47D0,
     $ 1.38D0, 1.26D0, 1.13D0, 1.05D0, 0.982D0,
     $ 0.914D0, 0.807D0, 0.721D0, 0.673D0, 0.631D0,
     $ 0.577D0, 0.525D0, 0.472D0, 0.437D0, 0.403D0,
     $ 0.370D0, 0.339D0/
C
      data ZERO,HALF,PW,FAC /0.D0, 5.D-1, 1.5D0, 1.D-17/
C
      data KILROY /.true./
C     !EJECT
C
      call HI ('ROMATIN')
C     !BEG
      if(KILROY) then
        KILROY = .false.
        call RIGEL (24, CON24)
        call RIGEL (72, CON72)
C
        do 100 I = 2,N
          D(I) = HALF*(Y(I)-Y(I-1))
  100   continue
      end if
C
      CI1 = ZERO
C
      if(TE.gt.ZERO) then
        TF = CON24/TE
        TP = TE**PW
        XM = (FAC*CON72)/TP
        F  = ZERO
C
        do 101 I = 2,N
          FP = F
          EX = TF*(Y(I)-Y(1))
          E  = exp(-EX)
          F  = S(I)*Y(I)*E
C
          CI1 = CI1+(F+FP)*D(I)
  101   continue
C
        CI1 = CI1*XM
      end if
C     !END
      call BYE ('ROMATIN')
C
      return
      end
