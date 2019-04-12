      subroutine OXUS
     $(P,FRS,S,N)
C
C     Rudolf Loeser, 1983 Jul 14
C---- Multiplies a "spherical" result by R**2.
C     !DASH
      save
C     !DASH
      real*8 FRS, P, S
      integer I, N
C     !DASH
      external HI, BYE
C
C               P(N), FRS(N), S(N)
      dimension P(*), FRS(*), S(*)
C
      call HI ('OXUS')
C     !BEG
      do 100 I = 1,N
        S(I) = P(I)*(FRS(I)**2)
  100 continue
C     !END
      call BYE ('OXUS')
C
      return
      end
