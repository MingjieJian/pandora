      subroutine SATAN2
     $(X,Y,RES)
C     Rudolf Loeser, 1990 Dec 14
C---- Computes atan2(x,y).
C     !DASH
      save
C     !DASH
      real*8 RES, X, Y
C     !COM
C---- FOCUS       as of 1998 Jun 02
      integer     KNTEFR
      dimension   KNTEFR(11)
      common      /FOCUS/ KNTEFR
C     Counts of calls to elementary function subroutines.
C     .
C
C     !BEG
      RES = atan2(X,Y)
      KNTEFR(11) = KNTEFR(11)+1
C     !END
C
      return
      end
