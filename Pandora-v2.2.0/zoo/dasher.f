      subroutine DASHER
     $(NO)
C
C     Rudolf Loeser, 1995 Feb 13
C---- Writes a full line of dashes in the file attached to unit "NO".
C     (See also STARER.)
C     !DASH
      save
C     !DASH
      integer NO
C     !DASH
      external RULER
C
C     !BEG
      if(NO.gt.0) then
        call RULER (NO,'-',1000000)
      end if
C     !END
C
      return
      end
