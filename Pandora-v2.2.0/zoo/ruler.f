      subroutine RULER
     $(NO,CHAR,KOUNT)
C
C     Rudolf Loeser, 1995 Feb 13
C---- Writes a line of "KOUNT" successive "CHAR"s
C     in the file attached to unit "NO".
C
C     The current line length is   127   characters;
C     this number occurs twice in the code below!
C     !DASH
      save
C     !DASH
      integer I, KOUNT, NO
      character CHAR*1
C     !DASH
      intrinsic min
C
C     !BEG
      if((NO.gt.0).and.(KOUNT.gt.0)) then
        write (NO,100) (CHAR,I=1,min(KOUNT,127))
  100   format(' ',127A1)
      end if
C     !END
C
      return
      end
