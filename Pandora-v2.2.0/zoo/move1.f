      subroutine MOVE1
     $(A,N,B)
C     Rudolf Loeser, 1986 Oct 03
C---- Special version of MOVED - q.v.
C     A and B both are arrys of length N.
C     !DASH
      save
C     !DASH
      real*8 A, B
      integer I, N
C     !DASH
      dimension A(*), B(*)
C
C     !BEG
      if(N.gt.0) then
        do 100 I = 1,N
          B(I) = A(I)
  100   continue
      end if
C     !END
C
      return
      end
