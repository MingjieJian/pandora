      subroutine LENA
     $(H,N,K,H1,H1M)
C
C     Rudolf Loeser, 1985 Jan 23
C---- Sets up data for printing, for TOBOL.
C     (This is version 2 of LENA.)
C     !DASH
      save
C     !DASH
      real*8 CON30, H, H1, H1M
      integer K, N
C     !DASH
      external MOVED, RIGEL, MOVE1, CONMUL, HI, BYE
C
C               H(N,K), H1(K), H1M(K)
      dimension H(N,*), H1(*), H1M(*)
C
C
      call HI ('LENA')
C     !BEG
      call MOVED  (H(1,1),N,K,H1,1,K)
C
      call MOVE1  (H1,K,H1M)
      call RIGEL  (30,CON30)
      call CONMUL (CON30,H1M,K)
C     !END
      call BYE ('LENA')
C
      return
      end
