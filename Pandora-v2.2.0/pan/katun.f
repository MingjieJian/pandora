      subroutine KATUN
     $(RCP,RNU,UJI,RJ,YW,KOOL, FA,F)
C
C     Rudolf Loeser, 1993 Feb 05
C---- Computes an integrand, for CRECY.
C     (This is version 2 of KATUN.)
C     !DASH
      save
C     !DASH
      real*8 F, FA, FAC, RCP, RJ, RNU, UJI, YW
      logical KOOL
C     !DASH
      external FLECK, DIVIDE, HI, BYE
C
      call HI ('KATUN')
C     !BEG
      if(KOOL) then
        FAC = RCP
      else
        call DIVIDE (RCP, RNU, FAC)
      end if
C
      call FLECK    (UJI, RJ, RNU, YW, FA)
      F = FA*FAC
C     !END
      call BYE ('KATUN')
C
      return
      end
