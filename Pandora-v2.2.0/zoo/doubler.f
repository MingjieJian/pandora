      subroutine DOUBLER
     $(NO)
C     Rudolf Loeser, 2000 Aug 16
C---- Writes a full line of equals in the file attached to unit "NO".
C     !DASH
      save
C     !DASH
      integer NO
C     !DASH
      external RULER
C
C     !BEG
      if(NO.gt.0) then
        call RULER (NO,'=',1000000)
      end if
C     !END
C
      return
      end
