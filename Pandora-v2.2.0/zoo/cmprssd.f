      subroutine CMPRSSD
     $(FLT,N,K,L,KOUNT)
C     Rudolf Loeser, 1981 Mar 20
C---- Given array FLT, of length N, and indices K and L,
C     such that 1 .le. K .le. L .le. N, which specify a set of
C     consecutive elements to be eliminated from FLT.
C     This routine computes KOUNT, 0 .lt. KOUNT .le. N,
C     the number of elements to be eliminated, and then,
C     as required, compresses the remaining elements of the array,
C     such that A-new(K to N-KOUNT) .eq. A-old(L+1 to N).
C---- Returns with KOUNT .le. 0 and FLT unchanged if L .lt. K.
C     !DASH
      save
C     !DASH
      real*8 FLT
      integer I, J, K, KOUNT, L, M, N
C     !DASH
      intrinsic min
C
      dimension FLT(*)
C
C     !BEG
      KOUNT = L-(K-1)
      if(KOUNT.gt.0) then
        KOUNT = min(KOUNT,N)
        if(L.lt.N) then
          I = L+1
          M = K-1
          do 100 J = I,N
            M = M+1
            FLT(M) = FLT(J)
  100     continue
        end if
      end if
C     !END
C
      return
      end
