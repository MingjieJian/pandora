      subroutine NEARLY
     $(A,N, CRIT,DELTA, KEQ)
C     Rudolf Loeser, 1993 Feb 16
C---- Given the array A(i), 1 .le. i .le. N, this routine
C     counts how many values of A are such that
C     (CRIT - DELTA) .le. A(i) .le. (CRIT + DELTA).
C     !DASH
      save
C     !DASH
      real*8 A, CRIT, DELTA, RHI, RLO
      integer I, KEQ, N
C     !DASH
      dimension A(*)
C
C     !BEG
      KEQ = 0
      if(N.gt.0) then
        RLO = CRIT-DELTA
        RHI = CRIT+DELTA
        do 100 I = 1,N
          if((A(I).ge.RLO).and.(A(I).le.RHI)) then
            KEQ = KEQ+1
          end if
  100   continue
      end if
C     !END
C
      return
      end
