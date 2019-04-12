      subroutine ESAN
     $(LU,IMAGE,LAB)
C
C     Rudolf Loeser, 1990 Apr 23
C---- Prints plot for FANDAR.
C     !DASH
      save
C     !DASH
      integer LU
      character IMAGE*(*), LAB*(*)
C     !DASH
      external ABJECT, LINER, KPRINT, HI, BYE
C
      call HI ('ESAN')
C     !BEG
      call ABJECT (LU)
      write (LU,100) LAB
  100 format(' ','Plot 1: velocities VE, VP, VH, V1, V2, V3, vs. ',A,
     $           '.')
      call LINER  (1,LU)
C
      call KPRINT (IMAGE,LU)
C     !END
      call BYE ('ESAN')
C
      return
      end
