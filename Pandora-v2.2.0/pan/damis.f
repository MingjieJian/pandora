      subroutine DAMIS
     $(DIDH,MYX,N,LIST,L,ILO,IHI)
C
C     Rudolf Loeser, 1991 Aug 22
C---- Selects Z-limits of dI/dh plot.
C     !DASH
      save
C     !DASH
      real*8 DIDH
      integer I, IHI, ILO, J, JHI, JLO, L, LIST, MYX, N
C     !DASH
      external  EGESTA, HI, BYE
      intrinsic min, max
C
C               DIDH(N,NW), MYX(NW), LIST(L)
      dimension DIDH(N,*),  MYX(*),  LIST(*)
C
      call HI ('DAMIS')
C     !BEG
      ILO = N
      IHI = 1
C
      do 100 I = 1,L
C
        if(LIST(I).lt.0) then
          J = -LIST(I)
          call EGESTA (DIDH(1,J),MYX(J),N,JLO,JHI)
          ILO = min(ILO,JLO)
          IHI = max(IHI,JHI)
        end if
C
  100 continue
C
      if(IHI.lt.(ILO+2)) then
        ILO = 1
        IHI = N
      end if
C     !END
      call BYE ('DAMIS')
C
      return
      end
