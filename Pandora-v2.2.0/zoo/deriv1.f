      subroutine DERIV1
     $(X,F,D1,N)
C
C     Rudolf Loeser, 1986 Mar 16
C---- Computes the first derivative of F(X).
C     !DASH
      save
C     !DASH
      real*8 D1, F, HALF, SL, SR, X
      integer I, M, N
C     !DASH
      external DIVVY
C
      dimension X(*), F(*), D1(*)
C
      data HALF /5.D-1/
C
C     !BEG
      if(N.ge.2) then
        M = N-1
        call DIVVY     ((F(2  )-F(1)),(X(2  )-X(1)),SR)
        D1(1) = SR
        if(N.gt.2) then
          do 100 I = 2,M
            SL = SR
            call DIVVY ((F(I+1)-F(I)),(X(I+1)-X(I)),SR)
            D1(I) = HALF*(SL+SR)
  100     continue
        end if
        D1(N) = SR
      end if
C     !END
C
      return
      end
