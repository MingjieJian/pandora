      subroutine HAND
     $(R,N,G,H,C,HND)
C
C     Rudolf Loeser, 1980 Nov 03
C---- Computes HND, for the H.S.E. calculation.
C     !DASH
      save
C     !DASH
      real*8 C, EX, G, H, HND, R
      integer I, N
C     !DASH
      external HI, BYE
C
C               G(N), H(N), HND(N)
      dimension G(*), H(*), HND(*)
C
      call HI ('HAND')
C     !BEG
      do 100 I = 1,N
        EX     = exp(C*H(I))
        HND(I) = R*G(I)*EX
  100 continue
C     !END
      call BYE ('HAND')
C
      return
      end
