      subroutine AUBREY
     $(N,Z,BETA,DZB,GRF,WONE,HEND,BETAR)
C
C     Rudolf Loeser, 1997 May 12
C---- Computes intermediates, for TARPON.
C     (This is version 2 of AUBREY.)
C     !DASH
      save
C     !DASH
      real*8 BETA, BETAR, DZB, GRF, HEND, Z
      integer N
      logical WONE
C     !DASH
      external PIDGE, YURABE, HI, BYE
C
C               BETAR(N), BETA(N), DZB(N), Z(N), GRF(N), HEND(N)
      dimension BETAR(*), BETA(*), DZB(*), Z(*), GRF(*), HEND(*)
C
      call HI ('AUBREY')
C     !BEG
C---- Fudge factor for GNV
      call PIDGE  (GRF, N, WONE)
C
C---- beta-ratio and its derivative, DZB
      call YURABE (N, Z, HEND, BETA, BETAR, DZB)
C     !END
      call BYE ('AUBREY')
C
      return
      end
