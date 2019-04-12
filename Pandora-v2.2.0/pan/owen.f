      subroutine OWEN
     $(NO)
C
C     Rudolf Loeser, 1982 Jul 30
C---- Prints a break, for ATOM.
C     !DASH
      save
C     !DASH
      integer NO
C     !DASH
      external LINER, HI, BYE
C
      call HI ('OWEN')
C     !BEG
      call LINER (1,NO)
      write (NO,100)
  100 format(' ',49X,7('----------'))
C     !END
      call BYE ('OWEN')
C
      return
      end
