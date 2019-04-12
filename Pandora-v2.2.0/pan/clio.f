      subroutine CLIO
     $(F,N,VEC)
C
C     Rudolf Loeser, 1997 Aug 07
C---- Computes function to be differentiated, for SIGMA.
C     (This is version 3 of CLIO.)
C     !DASH
      save
C     !DASH
      real*8 F, VEC
      integer I, N
C     !DASH
      external TRINCA, HI, BYE
C
C               F(N), VEC(N)
      dimension F(*), VEC(*)
C
      call HI ('CLIO')
C     !BEG
      do 100 I = 1,N
        VEC(I) = log(F(I))
  100 continue
      call TRINCA (VEC, N)
C     !END
      call BYE ('CLIO')
C
      return
      end
