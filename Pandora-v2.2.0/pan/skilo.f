      subroutine SKILO
     $(N,HEND,PALBET,PBETAL,PBETGM,PGMBET,H)
C
C     Rudolf Loeser, 1998 Jun 30
C---- Sets up H, for OBELISK.
C     !DASH
      save
C     !DASH
      real*8 H, HEND, PALBET, PBETAL, PBETGM, PGMBET
      integer I, N
C     !DASH
      external HI, BYE
C
C               H(2,2,N), PALBET(N), PBETAL(N), PBETGM(N), PGMBET(N),
      dimension H(2,2,*), PALBET(*), PBETAL(*), PBETGM(*), PGMBET(*),
C
C               HEND(N)
     $          HEND(*)
C
      call HI ('SKILO')
C     !BEG
      do 100 I = 1,N
        H(1,1,I) =  HEND(I)*PALBET(I)
        H(1,2,I) = -HEND(I)*PBETAL(I)
        H(2,1,I) =  HEND(I)*(PGMBET(I)-PALBET(I))
        H(2,2,I) =  HEND(I)*(PGMBET(I)+PBETGM(I)+PBETAL(I))
  100 continue
C     !END
      call BYE ('SKILO')
C
      return
      end
