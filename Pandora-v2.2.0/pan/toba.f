      subroutine TOBA
     $(W,WC,C,N,NMAX)
C
C     Rudolf Loeser, 1981 Nov 03
C---- Adds a weight matrix contribution.
C     !DASH
      save
C     !DASH
      real*8 C, CI, W, WC
      integer I, J, N, NMAX
C     !DASH
      external HI, BYE
C
C               W(NMAX,NMAX), WC(N,N), C(N)
      dimension W(NMAX,*),    WC(N,*), C(*)
C
      call HI ('TOBA')
C     !BEG
      do 101 I = 1,N
        CI = C(I)
        do 100 J = 1,N
          W(I,J) = W(I,J)+CI*WC(I,J)
  100   continue
  101 continue
C     !END
      call BYE ('TOBA')
C
      return
      end
