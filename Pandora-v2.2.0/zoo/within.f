      subroutine WITHIN
     $(XMIN,X,XMAX,KODE, YES)
C
C     Rudolf Loeser, 1995 May 05
C---- Returns YES = .true. only if X falls in the interval (XMIN,XMAX).
C     If the input value of KODE = 0, then the endpoints are
C                                          IN the interval;
C                           KODE = 1, then the endpoints are
C                                          NOT in the interval.
C     !DASH
      save
C     !DASH
      real*8 A, B, X, XMAX, XMIN
      integer KODE
      logical YES
C
C     !BEG
      if(XMIN.eq.XMAX) then
        YES = (X.eq.XMIN).and.(KODE.eq.0)
      else
        if(XMAX.gt.XMIN) then
          A = XMIN
          B = XMAX
        else
          A = XMAX
          B = XMIN
        end if
        if(KODE.eq.0) then
          YES = ((A.le.X).and.(X.le.B))
        else
          YES = ((A.lt.X).and.(X.lt.B))
        end if
      end if
C     !END
C
      return
      end
