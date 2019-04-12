      subroutine SPOWER
     $(X,Y,RES)
C     Rudolf Loeser, 1987 Jul 15
C---- Computes x**y.
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
      RES = X**Y
      KNTEFR(3) = KNTEFR(3)+1
C     !END
C
      return
      end
