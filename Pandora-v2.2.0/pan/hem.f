      subroutine HEM
     $(M,N,NL,BDIJ,BDRM)
C
C     Rudolf Loeser, 1987 Jan 05
C---- Computes the ratios B(J)/B(M), 1 .le. J .le. NL, for
C     "b from b-ratios" calculation.
C     (This is version 3 of HEM.)
C     !DASH
      save
C     !DASH
      real*8 BDIJ, BDRM
      integer I, J, M, N, NL
C     !DASH
      external BRAT, HI, BYE
C
C               BDIJ(N,NL), BDRM(N,NL)
      dimension BDIJ(*),    BDRM(N,*)
C
      call HI ('HEM')
C     !BEG
      do 101 J = 1,NL
        do 100 I = 1,N
          call BRAT (I,J,M,BDIJ,BDRM(I,J))
  100   continue
  101 continue
C     !END
      call BYE ('HEM')
C
      return
      end
