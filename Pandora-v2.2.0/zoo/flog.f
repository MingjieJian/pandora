      subroutine FLOG
     $(X,RES)
C     Rudolf Loeser, 1979 May 14
C---- Approximate Log10 in a double precision context:
C     it is computed in single precision.
C     !DASH
      save
C     !DASH
      real*8 CUTHI, CUTLO, RES, X, ZERO
      real*4 A, Y
C     !COM
C---- FOCUS       as of 1998 Jun 02
      integer     KNTEFR
      dimension   KNTEFR(11)
      common      /FOCUS/ KNTEFR
C     Counts of calls to elementary function subroutines.
C     .
C     !DASH
      external SLOG10
C
      data ZERO,CUTLO,CUTHI /0.D0, .5D-38, 1.5D38/
C
C     !BEG
      if(X.le.ZERO) then
        RES = ZERO
      else if((X.lt.CUTLO).or.(X.gt.CUTHI)) then
        call SLOG10 (X,RES)
      else
        Y = X
        A = log10(Y)
        RES = A
      end if
      KNTEFR(6) = KNTEFR(6)+1
C     !END
C
      return
      end
