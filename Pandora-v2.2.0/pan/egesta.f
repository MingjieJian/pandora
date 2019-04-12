      subroutine EGESTA
     $(DIDH,MYX,N,JLO,JHI)
C
C     Rudolf Loeser, 1991 Aug 22
C---- Scans a run of dI/dh for DAMIS.
C     !DASH
      save
C     !DASH
      real*8 CRIT, DIDH, THSND
      integer I, JHI, JLO, MYX, N
C     !DASH
      external HI, BYE
C
C               DIDH(N)
      dimension DIDH(*)
C
      data THSND /1.D3/
C
      call HI ('EGESTA')
C     !BEG
      CRIT = DIDH(MYX)/THSND
C
      JLO = 0
      do 100 I = 1,N
        JLO = JLO+1
        if(DIDH(JLO).gt.CRIT) then
          go to 101
        end if
  100 continue
C
  101 continue
C
      JHI = N+1
      do 102 I = 1,N
        JHI = JHI-1
        if(DIDH(JHI).gt.CRIT) then
          go to 103
        end if
  102 continue
C
  103 continue
C     !END
      call BYE ('EGESTA')
C
      return
      end
