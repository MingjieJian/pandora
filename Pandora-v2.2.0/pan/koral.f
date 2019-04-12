      subroutine KORAL
     $(N,NL,RND,PLK,TRM)
C
C     Rudolf Loeser, 1998 Jun 29
C---- Computes a diffusion term.
C     !DASH
      save
C     !DASH
      real*8 PLK, RND, TRM
      integer I, J, N, NL
C     !DASH
      external HI, BYE
C
C               RND(N,NL), PLK(N,NL), TRM(N)
      dimension RND(N,*),  PLK(N,*),  TRM(*)
C
      call HI ('KORAL')
C     !BEG
      do 101 I = 1,N
C
        TRM(I) = RND(I,1)*PLK(I,1)
        do 100 J = 2,NL
          TRM(I) = TRM(I)+RND(I,J)*PLK(I,J)
  100   continue
C
  101 continue
C     !END
      call BYE ('KORAL')
C
      return
      end
