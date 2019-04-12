      subroutine LOOKUD
     $(T,N,DELTA,X,K,LOOK)
C     Rudolf Loeser, 1979 Apr 12
C---- See remarks in "LOOKUP."
C     !DASH
      save
C     !DASH
      real*8 DELTA, T, X
      integer FLAG, I, K, LOOK, N
C     !DASH
      external COMPD
C
      dimension T(*)
C
C     !BEG
      LOOK = 1
      do 100 I = 1,N
        call COMPD (T(I),X,DELTA,FLAG)
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
