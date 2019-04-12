      subroutine JUDGEC
     $(A,I,J,FLAG)
C     Rudolf Loeser, 1979 Apr 04
C---- "COMPARE" routine, type CHARACTER
C     !DASH
      save
C     !DASH
      integer FLAG, I, J
      character A*(*)
C     !DASH
      external COMPC
C
      dimension A(*)
C
C     !BEG
      call COMPC (A(I),A(J),FLAG)
C     !END
C
      return
      end
