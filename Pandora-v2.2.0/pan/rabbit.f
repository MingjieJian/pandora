      subroutine RABBIT
     $(RUNION,N,RABRUN,RAB)
C
C     Rudolf Loeser, 1984 Nov 20
C---- Sets up RAB, for LTE populations.
C     (This is version 2 of RABBIT.)
C     !DASH
      save
C     !DASH
      real*8 RAB, RABRUN
      integer N
      logical RUNION
C     !DASH
      external ONE1, MOVE1, HI, BYE
C
C               RABRUN(N), RAB(N)
      dimension RABRUN(*), RAB(*)
C
      call HI ('RABBIT')
C     !BEG
      if(RUNION) then
        call MOVE1 (RABRUN,N,RAB)
      else
        call ONE1  (RAB,N)
      end if
C     !END
      call BYE ('RABBIT')
C
      return
      end
