      subroutine BRUNY
     $(I,KUD,LAB)
C
C     Rudolf Loeser, 1987 Nov 27
C---- Makes a "j"-label for printing CO-lines opacity data arrays.
C     !DASH
      save
C     !DASH
      integer I, I1, I2, KUD
      character LAB*10
C     !DASH
      external CHILOE, HI, BYE
C
      call HI ('BRUNY')
C     !BEG
      call CHILOE (I,KUD,I1,I2)
C
      if(I2.le.9) then
        write (LAB,100) I1,I2
  100   format(I3,' to ',I1,2X)
      else if(I2.le.99) then
        write (LAB,101) I1,I2
  101   format(I3,' to ',I2,1X)
      else
        write (LAB,102) I1,I2
  102   format(I3,' to ',I3)
      end if
C     !END
      call BYE ('BRUNY')
C
      return
      end
