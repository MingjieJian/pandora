      subroutine GAME
     $(CPJ,RCP,RNU,MRP,YW,RJ,UJ,ITAU,N,DETL,IRLS,KOOL,XNUJ,FA,FB,
     $ RLAI,RLBI,RLMI,RLJ)
C
C     Rudolf Loeser, 1982 Dec 14
C---- Computes RLJ(itau) AND RLM(itau),
C     (and FA, RLAJ(itau) and/or FB, RLBJ(itau), if needed).
C     (This is version 3 of GAME.)
C     !DASH
      save
C     !DASH
      real*8 CPJ, F, FA, FB, RCP, RJ, RL, RLAI, RLBI, RLJ, RLMI, RNU,
     $       UJ, XNUJ, YW
      integer IRLS, ITAU, MRP, N
      logical DETL, KOOL
C     !DASH
      external GATE, CRECY, FOREZ, ABORT, HI, BYE
C
C               RNU(MRX+1), RCP(MRX+1), UJ(N), YW(N,MRX+1), FB(N,MRX+1),
      dimension RNU(*),     RCP(*),     UJ(*), YW(*),       FB(*),
C
C               FA(N,MRX+1)
     $          FA(*)
C
      call HI ('GAME')
C     !BEG
      call GATE    (CPJ, XNUJ, RJ, RCP(MRP), RNU(MRP), UJ(ITAU), KOOL,
     $              RLMI, F)
C
      if(DETL.or.(IRLS.eq.1)) then
        call CRECY (MRP, RNU, RCP, YW, ITAU, N, RJ, UJ, KOOL, FA, RL)
        RLAI = F*RL+RLMI
      end if
C
      if(DETL.or.(IRLS.eq.2)) then
        call FOREZ (MRP, RNU, RCP, YW, ITAU, N, RJ, UJ, KOOL, FB, RL)
        RLBI = F*RL+RLMI
      end if
C
      if(IRLS.eq.1) then
        RLJ = RLAI
      else
        RLJ = RLBI
      end if
C     !END
      call BYE ('GAME')
C
      return
      end
