      subroutine KIRGIZ
     $(R1N,ZN,Z,N,EMU)
C
C     Rudolf Loeser, 1981 Sep 09
C---- Computes values of EMU = cos(THETA), for a ray passing the disk.
C     (See also "LEBED".)
C     !DASH
      save
C     !DASH
      real*8 DC, DI, DN, DN2, DZ, EMU, HALF, R1N, RT, UM, Z, ZERO, ZN
      integer I, J, K, M, N, NRP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT(12),HALF  )
C     !DASH
C     !EJECT
      external HI, BYE
C
C               Z(N), EMU(NRPMX)
      dimension Z(*), EMU(*)
C
      call HI ('KIRGIZ')
C     !BEG
      NRP = 2*N+5
C
      DC  = R1N+ZN
      DN  = DC-Z(N)
      DN2 = DN**2
C
      M = N-1
      J = 0
      K = NRP+1
      do 100 I = 1,M
        DI = DC-Z(I)
        RT = sqrt(DI**2-DN2)
        UM = RT/DI
        J  = J+1
        K  = K-1
        EMU(J) = +UM
        EMU(K) = -UM
  100 continue
C
      DZ = Z(N)-Z(M)
      do 101 I = 1,3
        DZ = HALF*DZ
        DI = DN+DZ
        RT = sqrt(DI**2-DN2)
        UM = RT/DI
        J  = J+1
        K  = K-1
        EMU(J) = +UM
        EMU(K) = -UM
  101 continue
C
      EMU(N+3) = ZERO
C     !END
      call BYE ('KIRGIZ')
C
      return
      end
