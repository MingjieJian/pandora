      subroutine JUDGED
     $(A,I,J,FLAG)
C     Rudolf Loeser, 1979 Apr 04
C---- "COMPARE" routine, type DOUBLE PRECISION
C     !DASH
      save
C     !DASH
      real*8 A, DELTA
      integer FLAG, I, J
C     !COM
      common /JUDGD/ DELTA
C     !DASH
      external COMPD
C
      dimension A(*)
C
C     !BEG
      call COMPD (A(I),A(J),DELTA,FLAG)
C     !END
C
      return
      end
