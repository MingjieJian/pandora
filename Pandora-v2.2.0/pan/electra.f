      subroutine ELECTRA
     $(N,R1N,Z,R,A)
C
C     Rudolf Loeser, 1981 Aug 25.
C---- Computes weights, A, (and intermediate R),
C     for integration over Shells.
C     !DASH
      save
C     !DASH
      real*8 A, HALF, R, R1N, RPI, RPIM, SR, Z
      integer I, N
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT(12),HALF  )
C     !DASH
      external HI, BYE
C
C               Z(N), R(N+1), A(N)
      dimension Z(*), R(*),   A(*)
C
      call HI ('ELECTRA')
C     !BEG
      SR   = R1N+Z(N)
      R(1) = SR-Z(1)
      RPIM = R(1)
      do 100 I = 2,N
        RPI  = SR-Z(I)
        R(I) = HALF*(RPI+RPIM)
        RPIM = RPI
  100 continue
C
      R(N+1) = R1N
      do 101 I = 1,N
        A(I) = (R(I)**2)-(R(I+1)**2)
  101 continue
C     !END
      call BYE ('ELECTRA')
C
      return
      end
