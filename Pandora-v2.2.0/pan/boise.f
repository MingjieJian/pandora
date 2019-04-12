      subroutine BOISE
     $(KAMB,N,NL,RND,PLK,SPKL,PALBET,IAB,PBETAL,IBA,PBETGM,IBG,
     $ PGMBET,IGB)
C
C     Rudolf Loeser, 1998 Jun 30
C---- Sets up rates (and status codes), for OBELISK.
C     !DASH
      save
C     !DASH
      real*8 PALBET, PBETAL, PBETGM, PGMBET, PLK, RND, SPKL
      integer IAB, IBA, IBG, IGB, KAMB, N, NL
C     !DASH
      external KORAL, MOVE1, HI, BYE
C
C               RND(N,NL), PLK(N,NL), PGMBET(N), PALBET(N), PBETAL(N),
      dimension RND(*),    PLK(*),    PGMBET(*), PALBET(*), PBETAL(*),
C
C               PBETGM(N), SPKL(N)
     $          PBETGM(*), SPKL(*)
C
      call HI ('BOISE')
C     !BEG
      if(KAMB.eq.2) then
        call KORAL (N,NL,RND,PLK,PALBET)
        call MOVE1 (SPKL,N,PBETAL)
        IAB = 2
        IBA = 2
        IBG = 1
        IGB = 1
C
      else if(KAMB.eq.3) then
        call KORAL (N,NL,RND,PLK,PBETGM)
        call MOVE1 (SPKL,N,PGMBET)
        IAB = 1
        IBA = 1
        IBG = 2
        IGB = 2
C
      end if
C     !END
      call BYE ('BOISE')
C
      return
      end
