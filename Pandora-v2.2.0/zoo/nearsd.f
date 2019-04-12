      subroutine NEARSD
     $(T,N,X,I)
C     Rudolf Loeser, 1990 Nov 21
C---- A driver for LOOKSD (q.v.)
C     In essence,
C     T is a table of length N, sorted in ascending order.
C     Upon return, the value of I will have been set such that
C     abs(T(I)-X) is as small as possible.
C     Upon return, I .lt. 1 indicates an error.
C     !DASH
      save
C     !DASH
      real*8 DL, DR, T, X, ZERO
      integer I, LOOK, N, NOTE
C     !DASH
      external LOOKSD
C
      dimension T(*)
C
      data ZERO /0.D0/
C
C     !BEG
      if(N.le.0) then
        I = -1
      else if (N.eq.1) then
        I =  1
      else
        call LOOKSD (T,N, ZERO, X, I,NOTE,LOOK)
        if(LOOK.eq.1) then
          if(NOTE.eq.2) then
            DL = X-T(I  )
            DR = T(I+1)-X
            if(DL.ge.DR) then
              I = I+1
            end if
          end if
        else if((LOOK.eq.2).or.(LOOK.eq.3)) then
          I = N
        else
          I = 1
        end if
      end if
C     !END
C
      return
      end
