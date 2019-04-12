      subroutine NOVEMBR
     $(TAB,NB,TABB,DELTA,K)
C
C     Rudolf Loeser, 2004 Jul 20
C---- Checks a table of built-in background line data.
C     (Tis is version 3 of NOVEMBR.)
C     !DASH
      save
C     !DASH
      real*8 DELTA, TAB, TABB
      integer I, K, KK, NB
C     !DASH
      external  COMPD, HI, BYE
      intrinsic abs
C
C               TAB(LDLMX), TABB(NBMX)
      dimension TAB(*),     TABB(*)
C
      call HI ('NOVEMBR')
C     !BEG
      K = 0
      do 100 I = 1,NB
        call COMPD (TAB(I), TABB(I), DELTA, KK)
        K = K+abs(KK)
  100 continue
C     !END
      call BYE ('NOVEMBR')
C
      return
      end
