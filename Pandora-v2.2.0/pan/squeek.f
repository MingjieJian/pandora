      subroutine SQUEEK
     $(KK,EMUX,BDI,N,NL,V,KOLEV)
C
C     Rudolf Loeser, 1974 Dec 09
C---- Makes V, for the Lyman calculation.
C     !DASH
      save
C     !DASH
      real*8 BDI, EMUX, V
      integer K, KK, KOLEV, N, NL
C     !DASH
      external MINNOW, HI, BYE
C
C               EMUX(N,KKX), BDI(N,NL), V(N,KKX)
      dimension EMUX(N,*),   BDI(N,*),  V(N,*)
C
      call HI ('SQUEEK')
C     !BEG
      do 100 K = 1,KK
        call MINNOW (N, EMUX(1,K), BDI(1,KOLEV), V(1,K))
  100 continue
C     !END
      call BYE ('SQUEEK')
C
      return
      end
