      subroutine MYSTAT
     $(NO)
C     Rudolf Loeser, 1987 Jul 15
C---- Provides math functions usage counts, on unit NO.
C     !DASH
      save
C     !DASH
      integer NO
C     !COM
C---- FOCUS       as of 1998 Jun 02
      integer     KNTEFR
      dimension   KNTEFR(11)
      common      /FOCUS/ KNTEFR
C     Counts of calls to elementary function subroutines.
C     .
C
C     !BEG
      if(NO.gt.0) then
        write (NO,100) KNTEFR
  100   format(' ','Counts of calls to elementary mathematical ',
     $             'functions subroutines.'//
     $         ' ','SEXP  ',I10,10X,'SEXP10',I10,10X,'SPOWER',I10/
     $         ' ','SLOG  ',I10,10X,'SLOG10',I10,10X,'FLOG  ',I10/
     $         ' ','SCOS  ',I10,10X,'SSIN  ',I10,10X,'SSQRT ',I10/
     $         ' ','SATAN ',I10,10X,'SATAN2',I10)
      end if
C     !END
C
      return
      end
