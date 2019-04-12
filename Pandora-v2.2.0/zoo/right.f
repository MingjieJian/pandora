      subroutine RIGHT
     $(A,B,M)
C     Rudolf Loeser, 1979 Jun 06
C---- Takes the contents of the character variable "A,"
C     and makes them right-justified in "B," of length M.
C     (This is version 2 of RIGHT.)
C     !DASH
      save
C     !DASH
      integer I, J, K, M, N
      character A*(*), B*(*), BLANK*1
C     !DASH
      intrinsic max, len
C
      data BLANK /' '/
C
C     !BEG
      N = len(A)
      K = 0
      J = N+1
      do 100 I = 1,N
        J = J-1
        if(A(J:J).ne.BLANK) then
          goto 101
        end if
        K = K+1
  100 continue
C
  101 continue
      B(1:M) = BLANK
      J = M-(N-K)+1
      J = max(J,1)
      B(J:M) = A(1:N)
C     !END
C
      return
      end
