      subroutine RELDIFF
     $(A,B,DIFF)
C
C     Rudolf Loeser, 1998 Mar 30
C---- Computes the relative difference between A and B:
C     DIFF = (A - B) / [ (abs(A) + abs(B))/2 ]
C     !DASH
      save
C     !DASH
      real*8 A, B, DIFF, TWO, XDEN, XNUM, ZERO
C     !DASH
      external  DIVVY
      intrinsic abs
C
      data ZERO,TWO /0.D0, 2.D0/
C
C     !BEG
      XNUM = TWO*(A-B)
      XDEN = abs(A)+abs(B)
      if((XNUM.eq.ZERO).and.(XDEN.eq.ZERO)) then
        DIFF = ZERO
      else
        call DIVVY (XNUM, XDEN, DIFF)
      end if
C     !END
C
      return
      end
