      subroutine EGAS
     $(AMUX,NMT,NMTS)
C
C     Rudolf Loeser, 1982 Jun 07
C---- Determines NMTS, for NASSAU.
C     !DASH
      save
C     !DASH
      real*8 AMUX, CRIT
      integer I, NMT, NMTS
C     !DASH
      external  HI, BYE
      intrinsic min
C
C               AMUX(NMT)
      dimension AMUX(*)
C
      data CRIT /-1.D-2/
C
      call HI ('EGAS')
C     !BEG
      NMTS = 0
      do 100 I = 1,NMT
        if(AMUX(I).lt.CRIT) then
          NMTS = NMTS+1
        end if
  100 continue
      NMTS = min(NMTS,26)
C     !END
      call BYE ('EGAS')
C
      return
      end
