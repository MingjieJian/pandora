      subroutine STONE
     $(NO,I,MR,A,TIT)
C
C     Rudolf Loeser, 1972 Mar 02
C---- Print routine for MINK-type arrays.
C     !DASH
      save
C     !DASH
      real*8 A
      integer I, J, MR, NO
      character TIT*4
C     !DASH
      external HI, BYE
C
C               A(MR)
      dimension A(*)
C
      call HI ('STONE')
C     !BEG
      write (NO,100) TIT,I,(A(J),J=1,MR)
  100 format(' ',A3,6X,I3,1P10E11.3/
     $      (' ',12X,       10E11.3))
C     !END
      call BYE ('STONE')
C
      return
      end
