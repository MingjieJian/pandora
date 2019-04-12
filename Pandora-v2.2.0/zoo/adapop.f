      subroutine ADAPOP
     $(PD)
C     Rudolf Loeser, 1986 Jul 29
C---- Pops the (non-empty) stack, for ADAIR.
C     !DASH
      save
C     !DASH
      real*8 DMAX, DMIN, FL, FLIM, FLM, FM, FR, FRM, GG, PD, XL, XLM,
     $       XM, XR, XRM
      integer KD, NF, NOUT
C     !COM
      common /ADACOMM/ XL,FL,XLM,FLM,XM,FM,XRM,FRM,XR,FR,
     $                 FLIM,DMAX,DMIN,GG,NOUT,KD,NF
C     !DASH
      dimension PD(6,*)
C
C     !BEG
      XL = PD(1,KD)
      FL = PD(2,KD)
      XM = PD(3,KD)
      FM = PD(4,KD)
      XR = PD(5,KD)
      FR = PD(6,KD)
      KD = KD-1
C     !END
C
      return
      end
