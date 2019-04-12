      subroutine ANABAR
     $(N,K,PHI,GTN,ABC)
C
C     Rudolf Loeser, 1985 Jan 23
C---- Computes Line Absorption Coefficient, for TOBOL.
C     (This is version 2 of ANABAR.)
C     !DASH
      save
C     !DASH
      real*8 ABC, GTN, PHI
      integer J, K, N
C     !DASH
      external ARRMUL, HI, BYE
C
C               PHI(N,K), GTN(N), ABC(N,K)
      dimension PHI(N,*), GTN(*), ABC(N,*)
C
      call HI ('ANABAR')
C     !BEG
      do 100 J = 1,K
        call ARRMUL (GTN,PHI(1,J),ABC(1,J),N)
  100 continue
C     !END
      call BYE ('ANABAR')
C
      return
      end
