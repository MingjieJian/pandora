      subroutine SEXP10
     $(X,RES)
C     Rudolf Loeser, 1987 Jul 15
C---- Computes 10**x.
C     !DASH
      save
C     !DASH
      real*8 OOM, RES, X
C     !COM
C---- FOCUS       as of 1998 Jun 02
      integer     KNTEFR
      dimension   KNTEFR(11)
      common      /FOCUS/ KNTEFR
C     Counts of calls to elementary function subroutines.
C     .
C     !DASH
      data OOM /2.30258509299404568402D0/
C
C     !BEG
      RES = exp(X*OOM)
      KNTEFR(2) = KNTEFR(2)+1
C     !END
C
      return
      end
