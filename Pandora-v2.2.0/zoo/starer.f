      subroutine STARER
     $(NO)
C
C     Rudolf Loeser, 2002 Jul 12
C---- Writes a full line of asterisks in the file attached to unit "NO".
C     (See also DASHER.)
C     !DASH
      save
C     !DASH
      integer NO
C     !DASH
      external RULER
C
C     !BEG
      if(NO.gt.0) then
        call RULER (NO,'*',1000000)
      end if
C     !END
C
      return
      end
