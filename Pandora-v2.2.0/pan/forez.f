      subroutine FOREZ
     $(MRP,RNU,RCP,YW,ITAU,N,ZJ,UJ,KOOL, FB,RL)
C
C     Rudolf Loeser, 1974 Jun 14
C---- Computes an RL integral.
C     !DASH
      save
C     !DASH
      real*8 EL, ER, FB, GL, GR, ONE, RAT, RCP, RL, RNU, RUJI, UJ, YW,
     $       ZERO, ZJ, ZU
      integer ITAU, J, MRP, N
      logical KOOL
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external DIVIDE, MIMIR, HI, BYE
C
C               RNU(MRX+1), RCP(MRX+1), YW(N,MRX+1), FB(N,MRX+1), UJ(N)
      dimension RNU(*),     RCP(*),     YW(N,*),     FB(N,*),     UJ(*)
C
      call HI ('FOREZ')
C     !BEG
      call DIVIDE     (ONE, UJ(ITAU), RUJI)
C
      RL = ZERO
      if(MRP.le.1) then
        FB(ITAU,1) = ZERO
      else
C
        call MIMIR    (RCP(1), RNU(1), UJ(ITAU), ZJ, YW(ITAU,1), KOOL,
     $                 FB(ITAU,1), GR, ER)
C
        do 100 J = 2,MRP
          GL = GR
          EL = ER
C
          call MIMIR  (RCP(J), RNU(J), UJ(ITAU), ZJ, YW(ITAU,J), KOOL,
     $                 FB(ITAU,J), GR, ER)
C
          call DIVIDE ((GR-GL), (RNU(J)-RNU(J-1)), RAT)
          ZU = RAT*RUJI
          RL = RL+(EL*(GL+ZU)-ER*(GR+ZU))
  100   continue
C
      end if
C     !END
      call BYE ('FOREZ')
C
      return
      end
