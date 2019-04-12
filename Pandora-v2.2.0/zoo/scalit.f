      subroutine SCALIT
     $(X,N,NMAX,C,R,Y)
C     Rudolf Loeser, 1983 Dec 20
C---- Matrix scaling utility routine.
C     !DASH
      save
C     !DASH
      real*8 C, R, X, Y
      integer I, J, N, NMAX
C     !DASH
      dimension X(NMAX,N), Y(NMAX,N), C(N), R(N)
C
C     !BEG
      if(N.gt.0) then
        do 101 J = 1,N
          do 100 I = 1,N
            Y(I,J) = X(I,J)*C(J)*R(I)
  100     continue
  101   continue
      end if
C     !END
C
      return
      end
