      subroutine EDITSET
     $(F,N,CRIT,KODE,MODE)
C     Rudolf Loeser, 1990 Jun 20
C---- Fixes "bad" values, for EDIT1.
C     !DASH
      save
C     !DASH
      real*8 CRIT, F, ONE, X, X1, X2, ZERO
      integer I, ISTART, J, K, KODE, MODE, N
C     !DASH
      external EDIFIND, SET1, LINT
C
      dimension F(N)
C
      data ZERO, ONE /0.D0, 1.D0/
C
C     !BEG
      ISTART = 1
  100 continue
C----   Find next "bad" sequence
C       J and K (J .lt. K) will be the indices of the nearest good
C       values that bracket a sequence of "bad" values
C
        call EDIFIND    (ISTART,N,F,CRIT,KODE,J,K)
C
        if(J.lt.N) then
          if(J.lt.1) then
C----       The beginning of F is "bad"
            call SET1   (F,(K-1),F(K))
C
            ISTART = K
            goto 100
C
          end if
          if(K.gt.N) then
C----       The end of F is "bad"
            call SET1   (F(J+1),(N-J),F(J))
          else
C----       'Normal' case: an interior sequence of values is "bad"
            X1 = ZERO
            X2 = K-J
            X  = ZERO
            do 101 I = (J+1),(K-1)
              X = X+ONE
              call LINT (X1,F(J),X2,F(K), MODE, X,F(I))
  101       continue
            if(K.lt.N) then
C
              ISTART = K
              goto 100
C
            end if
          end if
        end if
C     !END
C
      return
      end
