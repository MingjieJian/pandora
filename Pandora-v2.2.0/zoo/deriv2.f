      subroutine DERIV2
     $(X,F,D2,N)
C
C     Rudolf Loeser, 1980 Sep 26
C---- Computes the second derivative of F(X).
C     !DASH
      save
C     !DASH
      real*8 D2, DL, DR, F, TWO, X, XDEN, XNUM, ZERO
      integer I, M, N
C     !DASH
      external DIVVY
C
      dimension X(*), F(*), D2(*)
C
      data ZERO,TWO /0.D0, 2.D0/
C
C     !BEG
      if(N.ge.2) then
        M = N-1
        D2(1) = ZERO
        D2(N) = ZERO
        if(N.gt.2) then
          do 100 I = 2,M
            DR = X(I+1)-X(I)
            DL = X(I)-X(I-1)
            XNUM = TWO*(DR*(F(I-1)-F(I))+DL*(F(I+1)-F(I)))
            XDEN = DR*(DR+DL)*DL
            call DIVVY (XNUM,XDEN,D2(I))
  100     continue
        end if
      end if
C     !END
C
      return
      end
