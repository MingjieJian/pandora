      subroutine TITANIA
     $(N,TAUIJ,NRAD,TLO,THI,ILO,IHI)
C
C     Rudolf Loeser, 1982 May 06
C---- Finds approximate limits for TE-plot.
C     (This is version 5 of TITANIA.)
C     !DASH
      save
C     !DASH
      real*8 TAUIJ, THI, TLO
      integer IHI, ILO, J, JE, JS, N, NRAD
C     !DASH
      external  GAZA, HI, BYE
      intrinsic min, max
C
C               TAUIJ(N,NRAD)
      dimension TAUIJ(N,*)
C
      call HI ('TITANIA')
C     !BEG
      ILO = N+1
      IHI = 0
      do 100 J = 1,NRAD
        call GAZA (TAUIJ(1,J),N,TLO,THI,JS,JE)
        ILO = min(ILO,JS)
        IHI = max(IHI,JE)
  100 continue
C     !END
      call BYE ('TITANIA')
C
      return
      end
