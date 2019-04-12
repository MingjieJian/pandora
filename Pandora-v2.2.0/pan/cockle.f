      subroutine COCKLE
     $(N,M,HND,PND,PNN,PMAX)
C
C     Rudolf Loeser, 2007 Mar 28
C---- Normalizes and finds maximum, for MULATOR.
C     !DASH
      save
C     !DASH
      real*8 HND, PMAX, PND, PNN
      integer IMAX, J, M, N, jummy
C     !COM
C---- ULTIMA      as of 2004 Mar 09
      real*8      ZZLARGE,ZZSMALL,ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
      common      /ULTIMA/ ZZLARGE,ZZSMALL,
     $                     ZLNLARG,ZLNSMAL,ZL10LAR,ZL10SMA
C     Extreme values of floating point numbers range.
C     (See also subroutines SNAFU, SNUFFLE and FOOZLE.)
C     .
C     !DASH
      external  ARRDIV, MINMAXD, HI, BYE
      intrinsic max
C
C               PND(N,M), PNN(N,M), HND(N)
      dimension PND(N,*), PNN(N,*), HND(*)
C
      call HI ('COCKLE')
C     !BEG
      PMAX = -ZZLARGE
      do 100 J = 1,M
        call ARRDIV  (PND(1,J), HND, PNN(1,J), N)
        call MINMAXD (PNN(1,J), 1, N, jummy, IMAX)
        PMAX = max(PMAX, PNN(IMAX,J))
  100 continue
C     !END
      call BYE ('COCKLE')
C
      return
      end
