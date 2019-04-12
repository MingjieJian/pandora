      subroutine BEDWYR
     $(RLI,N,MRP,KSHL,RKM,RLM,FA,FB,RLA,RLB)
C
C     Rudolf Loeser, 1984 Jan 20
C---- Initializes computed arrays, for MANU.
C     !DASH
      save
C     !DASH
      real*8 FA, FB, RKM, RLA, RLB, RLI, RLM
      integer MRP, N, NM
      logical KSHL
C     !DASH
      external ZERO1, MOVE1, HI, BYE
C
C               RKM(N), RLM(N), FA(N,MRP), FB(N,MRP), RLI(N), RLA(N),
      dimension RKM(*), RLM(*), FA(*),     FB(*),     RLI(*), RLA(*),
C
C               RLB(N)
     $          RLB(*)
C
      call HI ('BEDWYR')
C     !BEG
      call ZERO1   (RKM,N )
      call ZERO1   (RLM,N )
C
      NM = N*MRP
      call ZERO1   (FA ,NM)
      call ZERO1   (FB ,NM)
C
      if(.not.KSHL) then
        call MOVE1 (RLI,N,RLA)
        call MOVE1 (RLI,N,RLB)
      end if
C     !END
      call BYE ('BEDWYR')
C
      return
      end
