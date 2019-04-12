      subroutine SEARCHR
     $(T,INC,N,X,K,NOTE,IRET)
C     Rudolf Loeser, 1991 Jan 30.
C     (This is version 2 of LKP)
C     (Originally written for CDC 6400, 1977 Oct 10)
C
C     This is a specialized version of subroutine "LOOKUP',
C     for real*4 operands.
C
C---- Given the table T comprising N unique elements, N .gt. 1,
C     sorted into ascending order, and the key X.
C
C---- Successive elements of "T" are stored in memory locations
C     separated by the constant stride INC, INC > 0, such that the
C     I'th element of "T" lives in T(II), where II=1+INC*(I-1).
C
C---- Upon normal return -
C     IRET=1, and the value of K is such that T(K) .le. X .lt. T(K+1),
C     1 .le. K .lt. N, and NOTE=1 if T(K) .eq. X, NOTE=2 if T(K) .lt. X.
C
C---- The following abnormal returns can occur -
C     IRET=2 if T(N) .eq. X,
C     IRET=3 if X .gt. T(N),
C     IRET=4 if X .lt. T(1).
C     (When the return is abnormal, neither K nor NOTE are set.)
C     !DASH
      save
C     !DASH
      real*4 T, X
      integer I, INC, IRET, J, K, KT, N, NOTE, NT
C     !DASH
      dimension T(*)
C     !EJECT
C
C     !BEG
      NT = 1+INC*(N-1)
      if((X.gt.T(1)).and.(X.lt.T(NT))) then
        IRET = 1
        I = 1
        J = N
  100   continue
        if((J-I).gt.1) then
          K  = (I+J)/2
          KT = 1+INC*(K-1)
          if(X.lt.T(KT)) then
            J = K
          else
            I = K
          end if
          goto 100
        else
          K = I
          NOTE = 2
          if(X.eq.T(KT)) NOTE = 1
        end if
      else if(X.lt.T(1)) then
        IRET = 4
      else if(X.eq.T(1)) then
        IRET = 1
        K    = 1
        NOTE = 2
      else if(X.eq.T(NT)) then
        IRET = 2
      else if(X.gt.T(NT)) then
        IRET = 3
      end if
C     !END
C
      return
      end
