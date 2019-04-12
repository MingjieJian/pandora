      subroutine REFILL
     $(A,B,N)
C     Rudolf Loeser, 1991 Aug 27
C---- Given arrays A and B, both of length N.
C     If all elements of B = 0, then moves A into B:
C     i.e. an "empty" B is "refilled" from A.
C     !DASH
      save
C     !DASH
      real*8 A, B
      integer N
      logical BZERO
C     !DASH
      external NAUGHTD, MOVE1
C
C               A(N), B(N)
      dimension A(*), B(*)
C
C     !BEG
      if(N.gt.0) then
        call NAUGHTD (B,1,N,BZERO)
        if(BZERO) then
          call MOVE1 (A,N,B)
        end if
      end if
C     !END
C
      return
      end
