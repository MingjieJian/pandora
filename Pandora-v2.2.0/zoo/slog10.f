      subroutine SLOG10
     $(X,RES)
C     Rudolf Loeser, 1987 Jul 15
C---- Computes log10(x).
C     !DASH
      save
C     !DASH
      real*8 EM, RES, X
C     !COM
C---- FOCUS       as of 1998 Jun 02
      integer     KNTEFR
      dimension   KNTEFR(11)
      common      /FOCUS/ KNTEFR
C     Counts of calls to elementary function subroutines.
C     .
C     !DASH
      data EM /0.43429448190325182765D0/
C
C     !BEG
      RES = (log(X))*EM
      KNTEFR(5) = KNTEFR(5)+1
C     !END
C
      return
      end
