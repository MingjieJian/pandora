      subroutine MATAI
     $(NO,WV)
C
C     Rudolf Loeser, 1991 Jul 18
C---- Prints an error message for ELMO.
C     !DASH
      save
C     !DASH
      real*8 WV
      integer NF, NO
      character LINE*12
C     !DASH
      external ENCODED, HI, BYE
C
      call HI ('MATAI')
C     !BEG
      call ENCODED (WV,LINE,12,12,1,NF)
      write (NO,100) LINE
  100 format(' ',3X,A12,5X,'No solution was attempted - bad data.')
C     !END
      call BYE ('MATAI')
C
      return
      end
