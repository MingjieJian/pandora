      subroutine SLOG
     $(X,RES)
C     Rudolf Loeser, 1987 Jul 15
C---- Computes log(x).
C     !DASH
      save
C     !DASH
      real*8 RES, X
C     !COM
C---- FOCUS       as of 1998 Jun 02
      integer     KNTEFR
      dimension   KNTEFR(11)
      common      /FOCUS/ KNTEFR
C     Counts of calls to elementary function subroutines.
C     .
C
C     !BEG
      RES = log(X)
      KNTEFR(4) = KNTEFR(4)+1
C     !END
C
      return
      end
