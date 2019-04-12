      subroutine CONADD
     $(CON,A,N)
C     Rudolf Loeser, 1987 Oct 02
C---- Adds CON to every element of A.
C     !DASH
      save
C     !DASH
      real*8 A, CON, ZERO
      integer I, N
C     !DASH
      dimension A(N)
C
      data ZERO /0.D0/
C
C     !BEG
      if((N.gt.0).and.(CON.ne.ZERO)) then
        do 100 I = 1,N
          A(I) = A(I)+CON
  100   continue
      end if
C     !END
C
      return
      end
