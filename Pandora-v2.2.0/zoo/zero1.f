      subroutine ZERO1
     $(A,N)
C     Rudolf Loeser, 1986 Oct 03
C---- Special version of ZEROD - q.v.
C     !DASH
      save
C     !DASH
      real*8 A, ZERO
      integer I, N
C     !DASH
      dimension A(*)
C
      data ZERO /0.D0/
C
C     !BEG
      if(N.gt.0) then
        do 100 I = 1,N
          A(I) = ZERO
  100   continue
      end if
C     !END
C
      return
      end
