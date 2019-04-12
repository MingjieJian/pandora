      subroutine ADAPUSH
     $(PD)
C     Rudolf Loeser, 1986 Jul 29
C---- Pushes the stack, for ADAIR.
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
      KD = KD+1
      PD(1,KD) = XM
      PD(2,KD) = FM
      PD(3,KD) = XRM
      PD(4,KD) = FRM
      PD(5,KD) = XR
      PD(6,KD) = FR
C     !END
C
      return
      end
