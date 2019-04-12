      subroutine STEEPLY
     $(X,D,N, KRET)
C     Rudolf Loeser, 1997 Feb 07
C---- Initializes the SMOTHER process.
C     !DASH
      save
C     !DASH
      real*8 D, X, ZERO
      integer I, KRET, N
C     !DASH
      dimension X(N), D(N)
C
      data ZERO /0.D0/
C
C     !BEG
      if(N.lt.5) then
        KRET = -1
        goto 101
      end if
C
      D(1) = ZERO
      do 100 I = 2,N
        D(I) = X(I)-X(I-1)
        if(D(I).le.ZERO) then
          KRET = -2
          goto 101
        end if
  100 continue
C
      KRET = 0
  101 continue
C     !END
C
      return
      end
