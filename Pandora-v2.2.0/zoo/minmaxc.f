      subroutine MINMAXC
     $(A,INC,N,IMIN,IMAX)
C
C     Rudolf Loeser, 1979 Apr 19
C---- See remarks in "SCANNER".
C     !DASH
      save
C     !DASH
      integer FLAG, IMAX, IMIN, INC, K, N
      character A*(*), DIV*1
C     !DASH
      external SCANC, COMPC
C
      dimension A(*)
C
      data DIV /'L'/
C
C     !BEG
      call SCANC   (A, INC, N, DIV, A(1), IMIN, IMAX,
     $              K, K, K, K)
C
      if(IMIN.ne.1) then
        call COMPC (A(1), A(IMIN), FLAG)
        if(FLAG.lt.0) then
          IMIN = 1
        end if
      end if
C
      if(IMAX.ne.1) then
        call COMPC (A(1), A(IMAX), FLAG)
        if(FLAG.gt.0) then
          IMAX = 1
        end if
      end if
C     !END
C
      return
      end
