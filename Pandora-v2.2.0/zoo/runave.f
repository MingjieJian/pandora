      subroutine RUNAVE
     $(A,N, W,M, B,GOOD)
C     Rudolf Loeser, 1991 Sep 24
C
C---- Given the table of values A of length N (N .gt. 0), and
C     given the table of weights W of length M (M .gt. 0 and M odd),
C
C     RUNAVE
C
C     computes the table B of length N, such that each element of
C     B is the central weighted running average of "M" elements of A.
C
C     For example, at interior points (i) with, say, M = 3,
C
C     B(i)=[W(1)*A(i-1) + W(2)*A(i) + W(3)*A(i+1)]/[W(1)+W(2)+W(3)].
C
C     Near both ends, where the terms "W*A" may involve nonexistent
C     elements of A, such terms are omitted from the numerator, and
C     the corresponding Ws are omitted from the denominator.
C
C     Upon return, GOOD = .false. if M or N do not satisfy the
C     conditions stated above (and B was therefore not altered
C     in any way); otherwise, GOOD = .true.
C     !DASH
      save
C     !DASH
      real*8 A, B, SD, SN, SW, W, ZERO
      integer I, J, K, L, LP, M, N
      logical GOOD
C     !DASH
      intrinsic mod
C
C               A(N), B(N), W(M)
      dimension A(*), B(*), W(*)
C
      data      ZERO /0.D0/
C     !EJECT
C
C     !BEG
      GOOD = (N.gt.0).and.(M.gt.0).and.(mod(M,2).eq.1)
      if(GOOD) then
        if(M.eq.1) then
C----     Trivial case
          do 100 I = 1,N
            B(I) = A(I)
  100     continue
        else
          if(N.eq.1) then
C----       Trivial case
            B(1) = A(1)
          else
C
            L  = M/2
            LP = L+1
            if(N.lt.M) then
C
C----         Special case: there is no interior region
              do 102 I  =1,N
                SN = ZERO
                SD = ZERO
                J  = I-LP
                do 101 K = 1,M
                  J = J+1
                  if((J.ge.1).and.(J.le.N)) then
                    SN = SN+W(K)*A(J)
                    SD = SD+W(K)
                  end if
  101           continue
                B(I) = SN/SD
  102         continue
C
            else
C     !EJECT
C----         General case: there is an interior region
              SW = ZERO
              do 103 K = 1,M
                SW = SW+W(K)
  103         continue
C
C----         Left end
              do 105 I = 1,L
                SN = ZERO
                SD = ZERO
                J  = I-LP
                do 104 K = 1,M
                  J = J+1
                  if(J.ge.1) then
                    SN = SN+W(K)*A(J)
                    SD = SD+W(K)
                  end if
  104           continue
                B(I) = SN/SD
  105         continue
C
C----         Interior region
              do 107 I = (L+1),(N-L)
                SN = ZERO
                J  = I-LP
                do 106 K = 1,M
                  J  = J+1
                  SN = SN+W(K)*A(J)
  106           continue
                B(I) = SN/SW
  107         continue
C
C----         Right end
              do 109 I = (N-L+1),N
                SN = ZERO
                SD = ZERO
                J  = I-LP
                do 108 K = 1,M
                  J = J+1
                  if(J.le.N) then
                    SN = SN+W(K)*A(J)
                    SD = SD+W(K)
                  end if
  108           continue
                B(I) = SN/SD
  109         continue
C
            end if
          end if
        end if
      end if
C     !END
C
      return
      end
