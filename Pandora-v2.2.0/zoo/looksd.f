      subroutine LOOKSD
     $(T,N,DELTA,X,K,NOTE,LOOK)
C
C     Rudolf Loeser, 1979 Apr 12
C---- See remarks in "LOOKUP."
C     !DASH
      save
C     !DASH
      real*8 DELTA, T, X
      integer FLAG, I, J, K, LOOK, N, NOTE
C     !DASH
      external COMPD, ABORT
C
      dimension T(*)
C     !EJECT
C
C     !BEG
      LOOK = 1
      call COMPD       (T(1), X, DELTA, FLAG)
      if(FLAG.eq.0) then
        K = 1
        NOTE = 1
      else if(FLAG.gt.0) then
        LOOK = 4
      else
        call COMPD     (T(N), X, DELTA, FLAG)
        if(FLAG.eq.0) then
          LOOK = 2
        else if(FLAG.lt.0) then
          LOOK = 3
        else
          NOTE = 2
          I = 1
          J = N
  100     continue
          if((J-I-1).le.0) then
            K = I
            call COMPD (T(K), X, DELTA, FLAG)
            if(FLAG.eq.0) then
              NOTE = 1
            else if(FLAG.gt.0) then
              write (*,101) FLAG
  101         format(' ','Error in LOOKSD: FLAG =',I12)
              call ABORT
            end if
          else
            K = (I+J)/2
            call COMPD (T(K), X, DELTA, FLAG)
            if(FLAG.lt.0) then
              I = K
              goto 100
            else if(FLAG.gt.0) then
              J = K
              goto 100
            else
              NOTE = 1
            end if
          end if
        end if
      end if
C     !END
C
      return
      end
