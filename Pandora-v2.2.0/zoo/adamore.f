      subroutine ADAMORE
     $(SUB,VEC)
C     Rudolf Loeser, 1986 Jul 28
C---- Provides additional left- and right-subinterval
C     midpoint data, for ADAIR.
C     !DASH
      save
C     !DASH
      real*8 DMAX, DMIN, FL, FLIM, FLM, FM, FR, FRM, GG, HALF, VEC, XL,
     $       XLM, XM, XR, XRM
      integer KD, NF, NOUT
C     !DASH
      common /ADACOMM/ XL,FL,XLM,FLM,XM,FM,XRM,FRM,XR,FR,
     $                 FLIM,DMAX,DMIN,GG,NOUT,KD,NF
C     !DASH
      external SUB
C
      dimension VEC(*)
C
      data HALF /5.D-1/
C
C     !BEG
      XLM = HALF*(XL+XM)
      call SUB (VEC,XLM,FLM)
      XRM = HALF*(XM+XR)
      call SUB (VEC,XRM,FRM)
      NF  = NF+2
C
      if(NOUT.gt.0) then
        write (NOUT,100) XL,XLM,XM,XRM,XR, NF,
     $                   FL,FLM,FM,FRM,FR, KD
  100   format(/' ',1P5E24.16,I7/' ',5E24.16,I7)
      end if
C     !END
C
      return
      end
