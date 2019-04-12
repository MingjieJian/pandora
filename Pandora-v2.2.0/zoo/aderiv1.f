      subroutine ADERIV1
     $(X,F,D1,N)
C     Rudolf Loeser, 1988 Jul 21
C---- Computes the first derivative of F(X) by an alternate method.
C     !DASH
      save
C     !DASH
      real*8 D1, F, X
      integer I, N
C     !DASH
      external DIVVY
C
      dimension X(N), F(N), D1(N)
C
C     !BEG
      if(N.ge.2) then
        call DIVVY     ((F(2  )-F(1  )),(X(2  )-X(1  )),D1(1))
        if(N.gt.2) then
          do 100 I=2,N-1
            call DIVVY ((F(I+1)-F(I-1)),(X(I+1)-X(I-1)),D1(I))
  100     continue
        end if
        call DIVVY     ((F(N  )-F(N-1)),(X(N  )-X(N-1)),D1(N))
      end if
C     !END
C
      return
      end
