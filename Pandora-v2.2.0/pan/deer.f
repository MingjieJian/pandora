      subroutine DEER
     $(N,NL,NSL,M,GMI,RLI,RKI,SPKL)
C
C     Rudolf Loeser, 1978 Jun 26
C---- Computes SPKL, a Supplementary Levels term.
C     !DASH
      save
C     !DASH
      real*8 GMI, R, RKI, RLI, SPKL
      integer I, J, M, N, NL, NSL
C     !DASH
      external ZERO1, DIVIDE, HI, BYE
C
C               GMI(N,NSL), RLI(N,NSL), RKI(N,NSL), SPKL(N)
      dimension GMI(N,*),   RLI(N,*),   RKI(N,*),   SPKL(*)
C
      call HI ('DEER')
C     !BEG
      call ZERO1      (SPKL,N)
C
      do 101 J = (NL+1),NSL
C
        do 100 I = 1,N
          call DIVIDE (GMI(I,J),GMI(I,M),R)
          SPKL(I) = SPKL(I)+R*(RLI(I,J)-RKI(I,J))
  100   continue
C
  101 continue
C     !END
      call BYE ('DEER')
C
      return
      end
