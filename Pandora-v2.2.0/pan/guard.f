      subroutine GUARD
     $(IS,IE,A,IMIN,IMAX)
C     Rudolf Loeser, 1997 Oct 27
C---- Returns the indices of the extrema of a subset of array A.
C     (This is version 2 of GUARD.)
C     !DASH
      save
C     !DASH
      real*8 A
      integer IE, IMAX, IMIN, IS, M, N
C     !DASH
      external  MINMAXD
C
      dimension A(*)
C
C     !BEG
      if((IS.lt.1).or.(IS.ge.IE)) then
        IMIN = IE
        IMAX = IE
      else
        M = IS-1
        N = IE-M
        call MINMAXD (A(IS),1,N, IMIN,IMAX)
        IMIN = IMIN+M
        IMAX = IMAX+M
      end if
C     !END
C
      return
      end
