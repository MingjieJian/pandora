      subroutine ZEROI
     $(A,INC,N)
C     Rudolf Loeser, 1979 Apr 18
C---- Sets every "INC"-th element of the array "A,"
C     for a total of "N" elements, equal to 0.
C     !DASH
      save
C     !DASH
      integer A, INC, N, ZERO
C     !DASH
      external SETI
C
      dimension A(*)
C
      data ZERO /0/
C
C     !BEG
      call SETI (A,INC,N,ZERO)
C     !END
C
      return
      end
