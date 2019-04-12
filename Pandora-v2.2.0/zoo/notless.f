      subroutine NOTLESS
     $(T,N,X,I)
C     Rudolf Loeser, 1989 Aug 31
C---- A driver for LOOKSD (q.v.).
C     In essence,
C     T is a table of length N, sorted in ascending order.
C     Upon return, the value of I will have been set such that
C     T(I) is the smallest value of T not less than X.
C     Upon return, I .lt. 1 indicates an error.
C     !DASH
      save
C     !DASH
      real*8 T, X, ZERO
      integer I, LOOK, N, NOTE
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
        if(X.gt.T(N)) then
          I = 0
        else
          call LOOKSD (T,N,ZERO,X,I,NOTE,LOOK)
          if((LOOK.lt.1).or.(LOOK.gt.4).or.(LOOK.eq.3)) then
            write (*,100) LOOK
  100       format(' ','Error in NOTLESS: LOOK =',I12)
            call ABORT
          else if(LOOK.eq.1) then
            if((NOTE.lt.1).or.(NOTE.gt.2)) then
              write (*,101) NOTE
  101         format(' ','Error in NOTLESS: NOTE =',I12)
              call ABORT
            else if(NOTE.eq.2) then
              I = I+1
            end if
          else if(LOOK.eq.2) then
            I = N
          else
            I = 1
          end if
        end if
      end if
C     !END
C
      return
      end
