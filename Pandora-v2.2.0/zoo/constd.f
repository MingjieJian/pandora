      subroutine CONSTD
     $(ARRAY,N,DELTA,SAME)
C
C     Rudolf Loeser, 2003 Jul 24
C---- Tells whether all elements of ARRAY, length N, are equal to
C     relative tolerance DELTA.
C     Returns SAME = .true. if yes, = .false. otherwise.
C     !DASH
      save
C     !DASH
      real*8 ARRAY, DELTA
      integer KEQ, N
      logical SAME
C     !DASH
      external RANGED, HI, BYE
C
C               ARRAY(N)
      dimension ARRAY(*)
C
C     !BEG
      call RANGED (ARRAY,1,N,DELTA,ARRAY(1),KEQ)
      SAME = KEQ.eq.N
C     !END
C
      return
      end
