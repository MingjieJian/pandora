      subroutine BARABAS
     $(I,DP,N,DPX,NRP,LDL)
C
C     Rudolf Loeser, 1989 Mar 17
C---- Makes "extended" DP table(s).
C     !DASH
      save
C     !DASH
      real*8 DP, DPX
      integer I, L, LDL, N, NRP
C     !DASH
      external BARBARA, HI, BYE
C
C               DP(N,LDLMX), DPX(NRPMX,LDLMX)
      dimension DP(N,*),     DPX(NRP,*)
C
      call HI ('BARABAS')
C     !BEG
      do 100 L = 1,LDL
        call BARBARA (DP(1,L),I,DPX(1,L))
  100 continue
C     !END
      call BYE ('BARABAS')
C
      return
      end
