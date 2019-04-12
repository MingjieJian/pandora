      subroutine CYPRIS
     $(CARIAG,PARRAY,PCOUNT,UNIT,GROUP,COUNT,PRINT,LINUM)
C     Rudolf Loeser, 1979 Jan 29
C---- Prints line "LINUM" of the string in "GROUP".
C     (Either 1 line will be printed, or 3 lines will be
C     overprinted: CARIAG contains the necessary carriage-control
C     characters, PARRAY the printing elements, and the value of
C     PCOUNT is either 1 or 3.)
C     !DASH
      save
C     !DASH
      integer COUNT, I, LINUM, PCOUNT, PRINT, UNIT
      character CARIAG*1, GROUP*(*), LINE*127, PARRAY*1
C     !DASH
      external  ARES
C
      dimension PARRAY(3), CARIAG(3)
C
C     !BEG
      do 101 I = 1,PCOUNT
        call ARES (GROUP,COUNT,PARRAY(I),PRINT,LINUM,LINE)
        write (UNIT,100) CARIAG(I),LINE
  100   format(A1,A127)
  101 continue
C     !END
C
      return
      end
