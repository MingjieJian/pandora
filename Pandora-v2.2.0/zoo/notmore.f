      subroutine NOTMORE
     $(T,N,X,I)
C     Rudolf Loeser, 1989 Aug 31
C---- A driver for LOOKSD (q.v.).
C     In essence,
C     T is a table of length N, sorted in ascending order.
C     Upon return, the value of I will have been set such that
C     T(I) is the largest value of T not more than X.
C     Upon return, I .lt. 1 indicates an error.
C     !DASH
      save
C     !DASH
      real*8 T, X, ZERO
      integer I, LOOK, N, jummy
C     !DASH
      external LOOKSD, ABORT
C
      dimension T(*)
C
      data ZERO /0.D0/
C
C     !BEG
      if(N.le.0) then
        I = -1
      else
        if(X.lt.T(1)) then
          I = 0
        else
          call LOOKSD (T,N,ZERO,X,I,jummy,LOOK)
          if((LOOK.lt.1).or.(LOOK.gt.3)) then
            write (*,100) LOOK
  100       format(' ','Error in NOTMORE: LOOK =',I12)
            call ABORT
          else if(LOOK.ne.1) then
            I = N
          end if
        end if
      end if
C     !END
C
      return
      end
