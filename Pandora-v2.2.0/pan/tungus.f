      subroutine TUNGUS
     $(N,K,WAVE,CDW,A,ABC,H,RF)
C
C     Rudolf Loeser, 1985 Jan 23
C---- Computes radiative force.
C     (This is version 2 of TUNGUS.)
C     !DASH
      save
C     !DASH
      real*8 A, ABC, CDW, CON37, FAC, H, RF, SUM, WAVE
      integer I, J, K, N
C     !DASH
      external RIGEL, HI, BYE
C
C               A(K), ABC(N,K), H(N,K), RF(N)
      dimension A(*), ABC(N,*), H(N,*), RF(*)
C
      call HI ('TUNGUS')
C     !BEG
      call RIGEL (37,CON37)
      FAC = CON37*CDW/(WAVE**2)
C
      do 101 I = 1,N
        SUM = A(1)*ABC(I,1)*H(I,1)
        do 100 J = 2,K
          SUM = SUM+A(J)*ABC(I,J)*H(I,J)
  100   continue
C
        RF(I) = FAC*SUM
  101 continue
C     !END
      call BYE ('TUNGUS')
C
      return
      end
