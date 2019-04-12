      subroutine LABFIL
     $(LABEL,LINE)
C
C     Rudolf Loeser, 2002 May 17
C---- Makes a 127-char line containing LABEL, with a fill of dashes.
C     !DASH
      save
C     !DASH
      character DASH*124, LABEL*(*), LINE*127
C     !DASH
      data DASH /'------------------------------------------------------
     $------------------------------------------------------------------
     $----'/
C
C     !BEG
      LINE(1:127) = LABEL//'   '//DASH
C     !END
C
      return
      end
