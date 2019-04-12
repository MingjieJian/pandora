      subroutine PARIS
     $(GROUP,COUNT,PARRAY,CARIAG,PCOUNT,PRINT,UNIT)
C     Rudolf Loeser, 1979 Jan 29
C---- Prints the Giant-Letter image of the string in "GROUP",
C     of length "COUNT".
C     !DASH
      save
C     !DASH
      integer COUNT, I, PCOUNT, PRINT, UNIT
      character BLANK*1, CARIAG*1, GROUP*(*), PARRAY*1
C     !DASH
      external CYPRIS
C
      dimension PARRAY(3), CARIAG(3)
C
      data BLANK /' '/
C
C     !BEG
C     Loop over all component print-lines of a Giant-Letter group
      do 100 I = 1,11
        call CYPRIS (CARIAG,PARRAY,PCOUNT,UNIT,GROUP,COUNT,PRINT,I)
        CARIAG(1) = BLANK
C       (The above step must be done because the carriage control
C       of all but the first of the 11 lines of the Giant-Letter
C       group must always be ' '; however, the Caller is offered
C       carriage-control-choice for the first group of a string of
C       Giant-Letter text. The proper initial setting of
C       CARIAG(1) is determined by GLAUCON, called from HECTOR.)
  100 continue
C     !END
C
      return
      end
