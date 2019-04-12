      subroutine DORIA
     $(N,NMT,AR,NMTS,ARS,IMUX)
C
C     Rudolf Loeser, 1982 Jun 07
C---- Sets up a subset array, for NASSAU.
C     !DASH
      save
C     !DASH
      real*8 AR, ARS
      integer IMUX, J, JS, N, NMT, NMTS
C     !DASH
      external MOVE1, HI, BYE
C
C               AR(N,NMT), ARS(N,NMTS), IMUX(NMTS)
      dimension AR(N,*),   ARS(N,*),    IMUX(*)
C
      call HI ('DORIA')
C     !BEG
      do 100 JS = 1,NMTS
        J = IMUX(JS)
        call MOVE1 (AR(1,J),N,ARS(1,JS))
  100 continue
C     !END
      call BYE ('DORIA')
C
      return
      end
