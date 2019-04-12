      subroutine JULIUS
     $(N,LG,CMU,CI)
C
C     Rudolf Loeser, 1982 Feb 03
C---- Sets up angle-integration weights, for CREMONA.
C     !DASH
      save
C     !DASH
      real*8 CI, CMU
      integer J, LG, N
C     !DASH
      external SET1, HI, BYE
C
C               CMU(LG), CI(N,LG)
      dimension CMU(*),  CI(N,*)
C
      call HI ('JULIUS')
C     !BEG
      do 100 J = 1,LG
        call SET1 (CI(1,J),N,CMU(J))
  100 continue
C     !END
      call BYE ('JULIUS')
C
      return
      end
