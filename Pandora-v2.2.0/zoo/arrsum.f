      subroutine ARRSUM
     $(A,N,SUM)
C     Rudolf Loeser, 1989 Jan 03
C---- Computes the sum of the elements of an array.
C     !DASH
      save
C     !DASH
      real*8 A, SUM, ZERO
      integer I, N
C     !DASH
      dimension A(N)
C
      data ZERO /0.D0/
C
C     !BEG
      SUM = ZERO
      if(N.gt.0) then
        do 100 I = 1,N
          SUM = SUM+A(I)
  100   continue
      end if
C     !END
C
      return
      end
