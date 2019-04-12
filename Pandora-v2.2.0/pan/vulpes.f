      subroutine VULPES
     $(N,Z,HE1,BETA,HE2K,HEND,Z2,Z3,ZXG,VEC,W,IW)
C
C     Rudolf Loeser, 1989 Sep 14
C---- Computes Helium ionization terms, for the diffusion calculation.
C     (This is version 2 of VULPES.)
C     !DASH
      save
C     !DASH
      real*8 BETA, HE1, HE2K, HEND, VEC, W, Z, Z2, Z3, ZXG
      integer IW, N
C     !DASH
      external ARRDIV, SIGMA, HI, BYE
C
      dimension W(*), IW(*)
C
C               Z(N), HE1(N), HE2K(N), BETA(N), Z2(N), ZXG(N), HEND(N),
      dimension Z(*), HE1(*), HE2K(*), BETA(*), Z2(*), ZXG(*), HEND(*),
C
C               Z3(N), VEC(N)
     $          Z3(*), VEC(*)
C
      call HI ('VULPES')
C     !BEG
C---- Compute Z2
      call ARRDIV (BETA,HE1 ,VEC,N)
      call SIGMA  (N,Z,VEC,Z2 ,'Z2' ,W,IW)
C---- Compute Z3
      call ARRDIV (HE2K,BETA,VEC,N)
      call SIGMA  (N,Z,VEC,Z3 ,'Z3' ,W,IW)
C---- Compute ZXG
      call ARRDIV (HE2K,HEND,VEC,N)
      call SIGMA  (N,Z,VEC,ZXG,'ZXG',W,IW)
C     !END
      call BYE ('VULPES')
C
      return
      end
