      subroutine EQUAL
     $(T,N,DELTA,X,I)
C
C     Rudolf Loeser, 2002 Aug 16
C---- A driver for LOOKSD (q.v.).
C     T is a table of length N and must be sorted in ascending order.
C
C     Upon return, the value of I will have been set such that
C     T(I) = X to tolerance DELTA.
C
C     Upon return, I = 0 indicates that X is not in T.
C     !DASH
      save
C     !DASH
      real*8 DELTA, T, X
      integer I, LOOK, N, NOTE
C     !DASH
      external LOOKSD, ABORT
C
      dimension T(*)
C
C     !BEG
      I = 0
      if(N.gt.0) then
        call LOOKSD (T,N,DELTA,X,I,NOTE,LOOK)
        if((LOOK.lt.1).or.(LOOK.gt.4)) then
          write (*,100) LOOK
  100     format(' ','EQUAL:  LOOK =',I12,
     $               ', which is not 1, 2, 3, or 4.')
          call ABORT
        else if(LOOK.eq.1) then
          if((NOTE.lt.1).or.(NOTE.gt.2)) then
            write (*,101) NOTE
  101       format(' ','EQUAL:  NOTE =',I12,', which is not 1 or 2.')
            call ABORT
          else if(NOTE.eq.2) then
            I = 0
          end if
        else if(LOOK.eq.2) then
          I = N
        else if(LOOK.ge.3) then
          I = 0
        end if
      end if
C     !END
C
      return
      end
