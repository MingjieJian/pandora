      subroutine MUSHAI
     $(I,NPP)
C
C     Rudolf Loeser, 1981 Nov 02
C---- Computes NPP(I), for BIHAR.
C     !DASH
      save
C     !DASH
      integer I, NP, NPP
C     !DASH
      external TATAR, HI, BYE
C
      call HI ('MUSHAI')
C     !BEG
      NPP = 0
      NP  = 0
  100 continue
        call TATAR (NP)
        if(NP.gt.I) then
          NPP = NPP+1
          goto 100
        end if
      continue
C     !END
      call BYE ('MUSHAI')
C
      return
      end
