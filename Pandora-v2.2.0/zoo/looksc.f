      subroutine LOOKSC
     $(T,N,X,K,NOTE,LOOK)
C     Rudolf Loeser, 1979 Apr 12
C---- See remarks in "LOOKUP."
C     !DASH
      save
C     !DASH
      integer FLAG, I, J, K, LOOK, N, NOTE
      character T*(*), X*(*)
C     !DASH
      external  COMPC, ABORT
C
      dimension T(*)
C
C     !BEG
      LOOK = 1
      call COMPC       (T(1),X,FLAG)
      if(FLAG.eq.0) then
        K = 1
        NOTE = 1
      else if(FLAG.gt.0) then
        LOOK = 4
C     !EJECT
      else
        call COMPC     (T(N),X,FLAG)
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
            call COMPC (T(K),X,FLAG)
            if(FLAG.eq.0) then
              NOTE = 1
            else if(FLAG.gt.0) then
              write (*,101) FLAG
  101         format(' ','Error in LOOKSC: FLAG =',I12)
              call ABORT
            end if
          else
            K = (I+J)/2
            call COMPC (T(K),X,FLAG)
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
