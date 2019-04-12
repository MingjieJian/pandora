      subroutine MIMIR
     $(RCP,RNU,UJI,ZJ,YW,KOOL, FB,G,E)
C
C     Rudolf Loeser, 1993 Feb 05
C---- Computes components of an integrand, for FOREZ.
C     (This is version 2 of FOREZ.)
C     !DASH
      save
C     !DASH
      real*8 E, EX, FAC, FB, G, RCP, RNU, UJI, YW, ZJ
      logical KOOL
C     !DASH
      external DIVIDE, HI, BYE
C
      call HI ('MIMIR')
C     !BEG
      if(KOOL) then
        FAC = RCP
      else
        call DIVIDE (RCP, RNU, FAC)
      end if
C
      FB = ZJ*(RNU**3)+YW
      G  = FB*FAC
C
      EX = exp(-UJI*RNU)
      call DIVIDE   (EX, UJI, E)
C     !END
      call BYE ('MIMIR')
C
      return
      end
