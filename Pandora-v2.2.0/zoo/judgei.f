      subroutine JUDGEI
     $(A,I,J,FLAG)
C     Rudolf Loeser, 1979 Apr 04
C---- "COMPARE" routine, type INTEGER
C     !DASH
      save
C     !DASH
      integer A, FLAG, I, INT, J
C     !COM
      common /JUDGI/ INT
C     !DASH
      external COMPI
C
      dimension A(*)
C
C     !BEG
      call COMPI (A(I),A(J),INT,FLAG)
C     !END
C
      return
      end
