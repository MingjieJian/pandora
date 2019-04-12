      subroutine SING4
     $(FLAG,KRET)
C     Rudolf Loeser, 1979 Apr 05
C---- Common push-stack code for "SING" routines
C     !DASH
      save
C     !DASH
      integer FLAG, I, IL, IU, J, K, KRET, L, M, NC, NR
C     !COM
      common  /SING01/ I,J,K,L,M,NC,NR,IU,IL
C     !DASH
      dimension IU(23), IL(23)
C
C     !BEG
      KRET = 1
      if((L-I).gt.(J-K)) then
        IL(M) = I
        IU(M) = L
        I     = K
      else
        IL(M) = K
        IU(M) = J
        J     = L
      end if
      M = M+1
      if(M.gt.23) then
        FLAG = -1
        KRET =  2
      end if
C     !END
C
      return
      end
