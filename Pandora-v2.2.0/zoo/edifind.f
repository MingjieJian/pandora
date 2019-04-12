      subroutine EDIFIND
     $(ISTART,N,F,CRIT,KODE,J,K)
C     Rudolf Loeser, 1990 Jun 20
C---- Finds a sequence of "bad" values, for EDITSET.
C     Upon return, J < K.
C     If J < 1, then the beginning of F is "bad";
C     if K > N, then the end of F is "bad".
C     !DASH
      save
C     !DASH
      real*8 CRIT, F
      integer I, IS, ISTART, J, K, KODE, L, M, N
      logical BAD
C     !DASH
      external EDITEST, ABORT
C
      dimension F(N)
C
C     !BEG
C---- Search for first "bad" point, F(L)
      IS = ISTART
      L  = 0
      do 100 I = IS,N
        call EDITEST   (F(I),CRIT,KODE,BAD)
        if(BAD) then
          L = I
          goto 101
        end if
  100 continue
C
  101 continue
      if(L.eq.0) then
C----   No more "bad" points
        J = N+1
        K = N+1
      else if(L.eq.N) then
C----   Only the last value of F is "bad" - no further search needed
        J = N-1
        K = N+1
      else
C----   Search for next (i.e. first following) good value, F(M)
        IS = L
        M  = 0
        do 102 I = IS,N
          call EDITEST (F(I),CRIT,KODE,BAD)
          if(.not.BAD) then
            M = I
            goto 103
          end if
  102   continue
C
  103   continue
C     !EJECT
        if(M.eq.0) then
C----     No other good value was found
          if(L.eq.1) then
C----       All values are "bad"
C           (Control cannot get here because of the test in EDIT1)
            write (*,104)
  104       format(' ','Error in EDIFIND:  this "cannot" happen.')
            call ABORT
          else
C----       The end of F is "bad"
            J = L-1
            K = N+1
          end if
        else
          if(L.eq.1) then
C----       The beginning of F is "bad"
            J = 0
            K = M
          else
C----       'Normal' case: an interior sequence is "bad"
            J = L-1
            K = M
          end if
        end if
      end if
C     !END
C
      return
      end
