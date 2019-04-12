      subroutine ONE1
     $(A,N)
C     Rudolf Loeser, 1998 Mar 27
C---- Special version of SETD - q.v.
C     !DASH
      save
C     !DASH
      real*8 A, ONE
      integer I, N
C     !DASH
      dimension A(*)
C
      data ONE /1.D0/
C
C     !BEG
      if(N.gt.0) then
        do 100 I = 1,N
          A(I) = ONE
  100   continue
      end if
C     !END
C
      return
      end
