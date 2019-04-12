      subroutine LOOKUI
     $(T,N,INT,X,K,LOOK)
C     Rudolf Loeser, 1979 Apr 12
C---- See remarks in "LOOKUP."
C     !DASH
      save
C     !DASH
      integer FLAG, I, INT, K, LOOK, N, T, X
C     !DASH
      external COMPI
C
      dimension T(*)
C
C     !BEG
      LOOK = 1
      do 100 I = 1,N
        call COMPI (T(I),X,INT,FLAG)
        if(FLAG.eq.0) then
          K = I
          goto 101
        end if
  100 continue
      LOOK = 2
  101 continue
C     !END
C
      return
      end
