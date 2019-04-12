      subroutine MINMAXI
     $(A,INC,N,IMIN,IMAX)
C
C     Rudolf Loeser, 1979 Apr 19
C---- See remarks in "SCANNER".
C     !DASH
      save
C     !DASH
      integer A, DELTA, DIV, FLAG, IMAX, IMIN, INC, K, N
C     !DASH
      external SCANI, COMPI
C
      dimension A(*)
C
      data DELTA,DIV /0, 0/
C
C     !BEG
      call SCANI   (A, INC, N, DELTA, DIV, A(1), IMIN, IMAX,
     $              K, K, K, K)
C
      if(IMIN.ne.1) then
        call COMPI (A(1), A(IMIN), DELTA, FLAG)
        if(FLAG.lt.0) then
          IMIN=1
        end if
      end if
C
      if(IMAX.ne.1) then
        call COMPI (A(1), A(IMAX), DELTA, FLAG)
        if(FLAG.gt.0) then
          IMAX=1
        end if
      end if
C     !END
C
      return
      end
