      subroutine ADATEST
     $(ZL,ZM,ZR,USE)
C     Rudolf Loeser, 1986 Jul 29
C---- Checks whether a subinterval is acceptable
C     by testing its midpoint's interpolated function value.
C     Sets USE = .false. if unacceptable.
C     !DASH
      save
C     !DASH
      real*8 DMAX, DMIN, DZ, FL, FLIM, FLM, FM, FR, FRM, GG, HALF, XL,
     $       XLM, XM, XR, XRM, ZERO, ZL, ZM, ZR, ZV
      integer KD, NF, NOUT
      logical USE
C     !COM
      common /ADACOMM/ XL,FL,XLM,FLM,XM,FM,XRM,FRM,XR,FR,
     $                 FLIM,DMAX,DMIN,GG,NOUT,KD,NF
C     !DASH
      intrinsic max, abs
C
      data ZERO, HALF /0.D0, 5.D-1/
C
C     !BEG
      ZV = HALF*(ZL+ZR)
      DZ = max(abs(ZV),abs(ZM))
      if(DZ.ne.ZERO) then
        if(((abs(ZV-ZM))/DZ).ge.FLIM) then
          USE = .false.
        end if
      end if
C     !END
C
      return
      end
