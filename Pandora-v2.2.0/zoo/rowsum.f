      subroutine ROWSUM
     $(A,N,NDIM,M1,M2,SUM)
C     Rudolf Loeser, 1992 Dec 23
C---- Given array A, with N rows and at least M2 columns
C     (column dimension NDIM), and the vector SUM of length N,
C     computes for each row of A the sum of columns M1 through M2,
C     inclusive, returning these sums in SUM.
C     !DASH
      save
C     !DASH
      real*8 A, SUM
      integer J, M1, M2, N, NDIM
C     !DASH
      external MOVE1, ARRADD
C
C               A(NDIM,M2), SUM(N)
      dimension A(NDIM,*),  SUM(*)
C
C     !BEG
      if((N.gt.0).and.(M2.ge.M1)) then
        call MOVE1      (A(1,M1),N,SUM)
        if(M2.gt.M1) then
          do 100 J = (M1+1),M2
            call ARRADD (A(1,J),SUM,SUM,N)
  100     continue
        end if
      end if
C     !END
C
      return
      end
