      subroutine PASUKA
     $(N3,N,W2,W3)
C
C     Rudolf Loeser, 1981 Oct 28
C---- Matrix manipulation for NIDABA.
C     W2(N+3,N+3)  ->  W3(N,N+3).
C     !DASH
      save
C     !DASH
      real*8 W2, W3
      integer J, N, N3, NM
C     !DASH
      external MOVE1, HI, BYE
C
C               W2(N3,N3), W3(N,N3)
      dimension W2(N3,*),  W3(N,*)
C
      call HI ('PASUKA')
C     !BEG
      NM = N-1
      do 100 J = 1,N3
        call MOVE1 (W2(1,J),NM,W3(1,J))
        W3(N,J) = W2(N3,J)
  100 continue
C     !END
      call BYE ('PASUKA')
C
      return
      end
