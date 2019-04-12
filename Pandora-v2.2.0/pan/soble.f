      subroutine SOBLE
     $(KAMB,N,G1,DIDG1,XN1,XNK,ARAT,BRAT,VEC,PALBET,PBETAL,PBETGM,
     $ PGMBET)
C
C     Rudolf Loeser, 1998 Jun 30
C---- Computes diffusion G1, for OBELISK.
C     !DASH
      save
C     !DASH
      real*8 ARAT, BRAT, G1, PALBET, PBETAL, PBETGM, PGMBET, VEC, XN1,
     $       XNK
      integer KAMB, N
      logical DIDG1
C     !DASH
      external  ARRDIV, MOVE1, LOBE, HI, BYE
C
C               G1(N), XN1(N), XNK(N), PALBET(N), PBETAL(N), PBETGM(N),
      dimension G1(*), XN1(*), XNK(*), PALBET(*), PBETAL(*), PBETGM(*),
C
C               PGMBET(N), ARAT(N), BRAT(N), VEC(N)
     $          PGMBET(*), ARAT(*), BRAT(*), VEC(*)
C
      call HI ('SOBLE')
C     !BEG
      if(KAMB.eq.2) then
        call ARRDIV (ARAT,BRAT,VEC,N)
        call LOBE   (N,XNK,XN1,VEC,PBETAL,PALBET, G1)
        DIDG1 = .true.
      else if(KAMB.eq.3) then
        call MOVE1  (BRAT,N,VEC)
        call LOBE   (N,XNK,XN1,VEC,PGMBET,PBETGM, G1)
        DIDG1 = .true.
      end if
C     !END
      call BYE ('SOBLE')
C
      return
      end
