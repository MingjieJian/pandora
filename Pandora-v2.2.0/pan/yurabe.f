      subroutine YURABE
     $(N,Z,HEND,BETA,BETAR,DZB)
C
C     Rudolf Loeser, 2001 Dec 18
C---- Computes beta-ratio and its derivative, DZB, for diffusion.
C     (This is version 2 of YURABE.)
C     !DASH
      save
C     !DASH
      real*8 BETA, BETAR, DZB, HEND, Z
      integer N
C     !DASH
      external ARRDIV, ADERIV1, HI, BYE
C
C               Z(N), HEND(N), BETA(N), BETAR(N), DZB(N)
      dimension Z(*), HEND(*), BETA(*), BETAR(*), DZB(*)
C
      call HI ('YURABE')
C     !BEG
      call ARRDIV  (BETA,HEND,BETAR,N)
      call ADERIV1 (Z,BETAR,DZB,N)
C     !END
      call BYE ('YURABE')
C
      return
      end
