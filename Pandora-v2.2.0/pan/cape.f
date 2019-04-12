      subroutine CAPE
     $(N,DELTA,DEE,ZI,Z1,Z2,Z3,ZT,ZXG,HE1,BETA,HE2K)
C
C     Rudolf Loeser, 1998 Apr 06
C---- Computes DELTAs, for Special N1-calculation, in the diffusion
C     calculations.
C     (This is version 6 of CAPE.)
C     !DASH
      save
C     !DASH
      real*8 BETA, DEE, DELTA, HE1, HE2K, HE31, HE32, ONE, T32, Z1, Z2,
     $       Z3, ZI, ZT, ZXG
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, HI, BYE
C
C               DELTA(7,N), DEE(4,5,N), HE2K(N), BETA(N), ZI(N), ZT(N),
      dimension DELTA(7,*), DEE(4,5,*), HE2K(*), BETA(*), ZI(*), ZT(*),
C
C               HE1(N), ZXG(N), Z1(N), Z2(N), Z3(N)
     $          HE1(*), ZXG(*), Z1(*), Z2(*), Z3(*)
C
      call HI ('CAPE')
C     !BEG
      do 100 I = 1,N
        call DIVIDE (HE2K(I),BETA(I),HE32)
        call DIVIDE (HE2K(I),HE1(I) ,HE31)
        T32 = ONE+HE32
C
        DELTA(1,I) = DEE(1,2,I)*Z1(I)+DEE(1,3,I)*Z2(I)+DEE(1,4,I)*Z3(I)
     $                               +DEE(1,5,I)*ZT(I)
C
        DELTA(2,I) = DEE(3,1,I)*ZI(I)+DEE(3,2,I)*Z1(I)+DEE(3,5,I)*ZT(I)
        DELTA(3,I) = DEE(4,1,I)*ZI(I)+DEE(4,2,I)*Z1(I)+DEE(4,5,I)*ZT(I)
C
        DELTA(4,I) = DELTA(2,I)-(DEE(3,3,I)*HE32-DEE(3,4,I)*T32)*ZXG(I)
        DELTA(5,I) = DELTA(3,I)-(DEE(4,3,I)*HE32-DEE(4,4,I)*T32)*ZXG(I)
C
        DELTA(6,I) = DELTA(2,I)+(DEE(3,3,I)*HE31+DEE(3,4,I))*ZXG(I)
        DELTA(7,I) = DELTA(3,I)+(DEE(4,3,I)*HE31+DEE(4,4,I))*ZXG(I)
  100 continue
C     !END
      call BYE ('CAPE')
C
      return
      end
