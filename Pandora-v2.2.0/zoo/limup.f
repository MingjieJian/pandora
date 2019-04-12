      subroutine LIMUP
     $(I,K,LIM)
C     Rudolf Loeser, 1979 Apr 20
C---- LIM will be the smallest integral multiple of K
C     which is not smaller than I.
C     K must be .gt. 0.
C     !DASH
      save
C     !DASH
      integer I, K, L, LIM, M
C     !DASH
      intrinsic abs
C
C     !BEG
      if(K.le.0) then
        L = I
      else
        M = abs(K)
        L = M*(I/M+1)
        if(I.le.0) then
          L = L-M
        end if
      end if
      LIM = L
C     !END
C
      return
      end
