      subroutine SWAPR
     $(A,NA,B,NB)
C     Rudolf Loeser, 1979 Apr 04
C---- Given the array A, of length NA, and the array B, of length NB.
C     Let M = min(NA,NB).
C     This routine exchanges the contents of the first M elements of
C     A and the corresponding elements of B.
C     !DASH
      save
C     !DASH
      real*4 A, B, TEMP
      integer I, M, NA, NB
C     !DASH
      intrinsic min
C
      dimension A(*), B(*)
C
C     !BEG
      M = min(NA,NB)
      if(M.gt.0) then
        do 100 I = 1,M
          TEMP = A(I)
          A(I) = B(I)
          B(I) = TEMP
  100   continue
      end if
C     !END
C
      return
      end
