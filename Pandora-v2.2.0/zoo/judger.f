      subroutine JUDGER
     $(A,I,J,FLAG)
C     Rudolf Loeser, 1979 Apr 04
C---- "COMPARE" routine, type REAL
C     !DASH
      save
C     !DASH
      real*4 A, DELTA
      integer FLAG, I, J
C     !COM
      common /JUDGR/ DELTA
C     !DASH
      external COMPR
C
      dimension A(*)
C
C     !BEG
      call COMPR (A(I),A(J),DELTA,FLAG)
C     !END
C
      return
      end
