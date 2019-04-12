      subroutine CYNTHIA
     $(N,Z,X,ZN,R1N)
C
C     Rudolf Loeser, 1981 Sep 08
C---- Sets up a table of X values, for spherical intensity calculation.
C     (This is version 3 of CYNTHIA.)
C     !DASH
      save
C     !DASH
      real*8 C1, CI, CMPKM, CON, DZ, H, H1, HALF, HN, R1N, RT, RZ, TWO,
     $       X, Y1, YI, Z, ZN
      integer I, J, K, M, N, NRP
C     !COM
C---- SHAMAN      as of 1998 Mar 18
      integer     MCONSH,MUNISH
      parameter   (MCONSH=18, MUNISH=11)
      real*8      PCON,TUNI
      dimension   PCON(MCONSH),TUNI(MUNISH)
      common      /SHAMAN1/ PCON
      common      /SHAMAN2/ TUNI
C     Physical constants, and other universal constants (see: KOSMOS).
      equivalence (TUNI( 5),CMPKM )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
      equivalence (DLIT( 3),TWO   )
C     !DASH
C     !EJECT
      external HI, BYE
C
C               Z(N), X(NRPMX)
      dimension Z(*), X(*)
C
      call HI ('CYNTHIA')
C     !BEG
      NRP = 2*N+5
C
      HN  = ZN-Z(N)
      CON = TWO*R1N+HN
      H1 = ZN-Z(1)
      RT = sqrt(CON+H1)
      C1 = CMPKM*RT
      RZ = sqrt(Z(N)-Z(1))
      Y1 = C1*RZ
C
      M = N-1
      J = 0
      K = NRP+1
      do 100 I = 1,M
        J = J+1
        K = K-1
        H  = ZN-Z(I)
        RT = sqrt(CON+H)
        CI = CMPKM*RT
        RZ = sqrt(Z(N)-Z(I))
        YI = CI*RZ
        X(J) = Y1-YI
        X(K) = Y1+YI
  100 continue
C
      DZ = Z(N)-Z(M)
      do 101 I = 1,3
        J = J+1
        K = K-1
        DZ = HALF*DZ
        RT = sqrt(CON+HN+DZ)
        CI = CMPKM*RT
        RZ = sqrt(DZ)
        YI = CI*RZ
        X(J) = Y1-YI
        X(K) = Y1+YI
  101 continue
      X(N+3) = Y1
C     !END
      call BYE ('CYNTHIA')
C
      return
      end
