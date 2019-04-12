      subroutine SCRUNCH
     $(KE,AE,NE,A,N)
C
C     Rudolf Loeser, 1997 Dec 22
C---- Collapses the table of partial integrals, by summing
C     them appropriately, for SOUPCON.
C     !DASH
      save
C     !DASH
      real*8 A, AE, SUM
      integer I, J, K, KE, L, N, NE
C     !DASH
      external HI, BYE
C
C               AE(NE), A(N)
      dimension AE(*),  A(*)
C
      call HI ('SCRUNCH')
C     !BEG
      K = KE+1
      J = 1
      N = 1
      A(N) = AE(J)
      do 101 I = (1+K),NE,K
        J = J+1
        SUM = AE(J)
        do 100 L = 1,KE
          J = J+1
          SUM = SUM+AE(J)
  100   continue
        N = N+1
        A(N) = SUM
  101 continue
C     !END
      call BYE ('SCRUNCH')
C
      return
      end
