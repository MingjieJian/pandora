      subroutine BUSH
     $(X,INCX,F,INCF,A,INCA,N)
C     Rudolf Loeser, 1979 Apr 11.
C     (Originally written for CDC 6400, 1972 Jan 24)
C
C---- Running integrals with HELENA.
C
C---- Successive elements of "X" are stored in memory locations
C     separated by the constant stride INCX, INCX > 0, such that the
C     I'th element of "X" lives in X(II), where II=1+INCX*(I-1).
C
C---- Successive elements of "F" are stored in memory locations
C     separated by the constant stride INCF, INCF > 0, such that the
C     I'th element of "F" lives in F(II), where II=1+INCF*(I-1).
C
C---- Successive elements of "A" are stored in memory locations
C     separated by the constant stride INCA, INCA > 0, such that the
C     I'th element of "A" lives in A(II), where II=1+INCA*(I-1).
C     !DASH
      save
C     !DASH
      real*8 A, F, SUM, X, ZERO
      integer INCA, INCF, INCX, N
C     !DASH
      external  HELENA, BUTTEM
C
      dimension X(*), F(*), A(*)
C
      data ZERO /0.D0/
C
C     !BEG
      if(N.gt.1) then
        call HELENA (X,INCX,F,INCF,A,INCA,N,SUM)
        call BUTTEM (A,INCA,N,SUM)
      else
        if(N.gt.0) then
          A(1) = ZERO
        end if
      end if
C     !END
C
      return
      end
