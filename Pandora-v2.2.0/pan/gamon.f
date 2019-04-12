      subroutine GAMON
     $(TIT,I1,I2, LABEL)
C
C     Rudolf Loeser, 1999 Sep 28
C---- Makes a label, for ALFALFA.
C     (This is version 2 of GAMON.)
C     !DASH
      save
C     !DASH
      integer I1, I2
      character LABEL*20, TIT*8
C     !DASH
      external HI, BYE
C
      call HI ('GAMON')
C     !BEG
      LABEL = TIT
      if(I1.ne.0) then
        if(I2.ne.0) then
          write (LABEL(9:),100) I1,I2
  100     format(2I6)
        else
          write (LABEL(9:),101) I1
  101     format(I6,6X)
        end if
      end if
C     !END
      call BYE ('GAMON')
C
      return
      end
