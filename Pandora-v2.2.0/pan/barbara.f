      subroutine BARBARA
     $(F,N,FX)
C
C     Rudolf Loeser, 1981 Sep 08
C---- Makes an extended table of F, along a particular ray.
C     (This is version 5 of BARBARA.)
C     !DASH
      save
C     !DASH
      real*8 A, B, F, FM, FN, FX
      integer I, II, J, M5, N, N3
C     !DASH
      external HI, BYE
C
C               F(N), FX(2*N+5)
      dimension F(*), FX(*)
C
      dimension A(3), B(3)
C
      data      A /5.D-1, 2.5D-1, 1.25D-1/
      data      B /1.D0,  3.D0,   7.D0/
C
      call HI ('BARBARA')
C     !BEG
      M5 = 2*N+5
      N3 = N+3
C
      J = 0
      I = M5+1
C
      do 100 II = 1,(N-1)
        J = J+1
        I = I-1
        FX(J) = F(II)
        FX(I) = F(II)
  100 continue
C
      FN = F(N  )
      FM = F(N-1)
C
      do 101 II = 1,3
        J = J+1
        I = I-1
        FX(J) = A(II)*(FM+B(II)*FN)
        FX(I) = FX(J)
  101 continue
C
      FX(N3)=FN
C     !END
      call BYE ('BARBARA')
C
      return
      end
