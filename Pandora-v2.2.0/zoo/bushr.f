      subroutine BUSHR
     $(X,INCX,F,INCF,A,INCA,N)
C     Rudolf Loeser, 1992 Aug 07
C     (Originally written for CDC 6400, 1972 Jan 24)
C
C---- Running integrals with HELENA,
C     for real*4 operands.
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
      real*4 A, F, SUM, X, ZERO
      integer I, IA, INCA, INCF, INCX, N, NA, NMO
C     !DASH
      external  HELENAR
C
      dimension X(*), F(*), A(*)
C
      data ZERO /0.E0/
C
C     !BEG
      if(N.gt.1) then
        call HELENAR (X,INCX,F,INCF,A,INCA,N,SUM)
        if(N.ge.4) then
          NMO = N-1
          IA  = 1+INCA
          do 100 I = 3,NMO
            IA    = IA+INCA
            A(IA) = A(IA-INCA)+A(IA)
  100     continue
          NA    = 1+INCA*NMO
          A(NA) = SUM
        end if
      else
        if(N.gt.0) then
          A(1) = ZERO
        end if
      end if
C     !END
C
      return
      end
