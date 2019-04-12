      subroutine SATAN
     $(X,RES)
C     Rudolf Loeser, 1990 Dec 14
C---- Computes atan(x).
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
      RES = atan(X)
      KNTEFR(10) = KNTEFR(10)+1
C     !END
C
      return
      end
