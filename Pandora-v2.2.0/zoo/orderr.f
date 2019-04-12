      subroutine ORDERR
     $(A,PNT,N,W)
C     Rudolf Loeser, 1979 Apr 11
C---- Permutes the array "A," of length "N," into canonical
C     (e.g. sorted) order, as indicated by the
C     canonical pointer array "PNT."
C     "W" is working storage, also of length "N."
C---- The values of "PNT" signify order as follows:
C     if   J = PNT(I),
C     then A[output](I) = A[input](J).
C     !DASH
      save
C     !DASH
      real*4 A, W
      integer I, J, N, PNT
C     !DASH
      external MOVER
C
      dimension PNT(N), A(N), W(N)
C
C     !BEG
      do 100 I = 1,N
        J = PNT(I)
        W(I) = A(J)
  100 continue
      call MOVER (W,1,N,A,1,N)
C     !END
C
      return
      end
