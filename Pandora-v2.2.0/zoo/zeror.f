      subroutine ZEROR
     $(A,INC,N)
C     Rudolf Loeser, 1979 Apr 18
C---- Sets every "INC"-th element of the array "A,"
C     for a total of "N" elements, equal to 0.
C     !DASH
      save
C     !DASH
      real*4 A, ZERO
      integer INC, N
C     !DASH
      external SETR
C
      dimension A(*)
C
      data ZERO /0.E0/
C
C     !BEG
      call SETR (A,INC,N,ZERO)
C     !END
C
      return
      end
