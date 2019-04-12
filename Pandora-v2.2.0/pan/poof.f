      subroutine POOF
     $(N,NSL,RKI,IQRK,CQIN,PKS,NO)
C
C     Rudolf Loeser, 1980 Mar 12
C---- Accounts for K-Shell ionization.
C     !DASH
      save
C     !DASH
      real*8 CQIN, PKS, RKI
      integer IQRK, N, NO, NSL
C     !DASH
      external FOPO, ZEUMO, LEI, HI, BYE
C
C               RKI(N,NSL), IQRK(NSL), CQIN(N), PKS(N)
      dimension RKI(*),     IQRK(*),   CQIN(*), PKS(*)
C
      call HI ('POOF')
C     !BEG
C---- Compute
      call FOPO  (N, NSL, RKI, IQRK, CQIN, PKS)
C
C---- Print
      call ZEUMO (N, NSL, RKI, IQRK, CQIN, PKS, NO)
C     !END
      call BYE ('POOF')
C
      return
      end
