      subroutine PIGWEED
     $(LU,NMAX,KMAX)
C
C     Rudolf Loeser, 1989 Jun 21
C---- Returns the maximum values of N and K occurring
C     in a P.R.D. Jnu restart file.
C     !DASH
      save
C     !DASH
      real*8 dummy
      integer K, KMAX, LU, N, NMAX, jummy
      logical GOOD
C     !DASH
      external  THRASH, CABBAGE, HI, BYE
      intrinsic max
C
      call HI ('PIGWEED')
C     !BEG
      rewind LU
C
      NMAX = 0
      KMAX = 0
  100 continue
        call THRASH  (LU, N, K, GOOD)
        if(.not.GOOD) goto 101
        NMAX = max(N,NMAX)
        KMAX = max(K,KMAX)
        call CABBAGE (LU, 3, N, K, jummy, jummy, dummy, dummy, dummy)
        goto 100
C
  101 continue
C
      rewind LU
C     !END
      call BYE ('PIGWEED')
C
      return
      end
