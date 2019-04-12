      subroutine RACES
     $(K,A,V,VP,RII,GII)
C
C     Rudolf Loeser, 2005 Feb 17
C---- Computes tables of RII(V,VP) and GII(V,VP),
C     for damping parameter A,
C     for a table of V values of length K.
C
C     Uses subroutines provided by George Rybicki.
C
C     !DASH
      save
C     !DASH
      real*8 A, GII, PHI, RII, V, VP
      integer I, K
C     !DASH
      external RIIA, RVOIGT, DIVVY
C
C               V(K), RII(K), GII(K)
      dimension V(*), RII(*), GII(*)
C
C     !BEG
      call RVOIGT  (VP, A, PHI)
      do 100 I = 1,K
        call RIIA  (V(I), VP, A, RII(I))
        call DIVVY (RII(I), PHI, GII(I))
  100 continue
C     !END
C
      return
      end
