      subroutine CONMUL
     $(CON,A,N)
C     Rudolf Loeser, 1987 Oct 02
C---- Multiplies every element of A by CON.
C     !DASH
      save
C     !DASH
      real*8 A, CON, ONE
      integer I, N
C     !DASH
      dimension A(N)
C
      data ONE /1.D0/
C
C     !BEG
      if((N.gt.0).and.(CON.ne.ONE)) then
        do 100 I = 1,N
          A(I) = A(I)*CON
  100   continue
      end if
C     !END
C
      return
      end
