      subroutine DEBRIS
     $(XI,K,CDW,DL)
C
C     Rudolf Loeser, 1983 Dec 05
C---- Computes Delta-Lambda.
C     (This is version 2 of DEBRIS.)
C     !DASH
      save
C     !DASH
      real*8 CDW, DL, XI
      integer K
C     !DASH
      external MOVE1, CONMUL, HI, BYE
C
C               XI(K), DL(K)
      dimension XI(*), DL(*)
C
      call HI ('DEBRIS')
C     !BEG
      call MOVE1  (XI, K, DL)
      call CONMUL (CDW, DL, K)
C     !END
      call BYE ('DEBRIS')
C
      return
      end
