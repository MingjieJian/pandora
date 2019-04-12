      subroutine KANKER
     $(N,MRHO,RHOS,RHOJ,RHOW,RTIT,ARHO)
C
C     Rudolf Loeser, 2003 Apr 09
C---- Sets up RHO values for printing, for KRAKEN.
C     (See also REKKAN.)
C     !DASH
      save
C     !DASH
      real*8 ARHO, RHOJ, RHOS, RHOW
      integer MRHO, N
      character RTIT*4, TIT*4
C     !DASH
      external MOVE1, HI, BYE
C
C               RHOS(N), RHOJ(N), RHOW(N), RTIT(4), ARHO(N,3)
      dimension RHOS(*), RHOJ(*), RHOW(*), RTIT(*), ARHO(N,*)
C
      dimension TIT(3)
C
      data TIT /'RHOS', 'RHOJ', 'RHOW'/
C
      call HI ('KANKER')
C     !BEG
      RTIT(4) = TIT(MRHO+1)
C
      RTIT(1) = TIT(1)
      call MOVE1 (RHOS, N, ARHO(1,1))
C
      RTIT(2) = TIT(2)
      call MOVE1 (RHOJ, N, ARHO(1,2))
C
      RTIT(3) = TIT(3)
      call MOVE1 (RHOW, N, ARHO(1,3))
C     !END
      call BYE ('KANKER')
C
      return
      end
