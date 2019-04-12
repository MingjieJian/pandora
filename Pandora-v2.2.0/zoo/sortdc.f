      subroutine SORTDC
     $(A,N,JUDGE,KSORT,KDIST)
C     Rudolf Loeser, 1979 Apr 05
C---- See remarks in "SORTED."
C     !DASH
      save
C     !DASH
      integer I, IFLG, KDEC, KDIST, KINC, KSORT, M, N
      character A*(*)
C     !DASH
      external JUDGE
C
      dimension A(*)
C
C     !BEG
      KDIST = 1
      if(N.le.1) then
        KSORT = 1
      else
        KSORT = 0
        KINC  = 0
        KDEC  = 0
        do 100 I = 2,N
          call JUDGE (A,I-1,I,IFLG)
          if(IFLG.lt.0) then
            KINC = KINC+1
          else if(IFLG.gt.0) then
            KDEC = KDEC+1
          else
            KINC  = KINC+1
            KDEC  = KDEC+1
            KDIST = 0
          end if
  100   continue
        M = N-1
        if(KDEC.eq.M) then
          KSORT = -1
        else if(KINC.eq.M) then
          KSORT = +1
        end if
      end if
C     !END
C
      return
      end
