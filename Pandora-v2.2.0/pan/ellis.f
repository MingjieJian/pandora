      subroutine ELLIS
     $(RCP,RNU,YW,KOOL,F)
C
C     Rudolf Loeser, 1993 Feb 05
C---- Computes an integrand, for DURAS.
C     !DASH
      save
C     !DASH
      real*8 F, FAC, RCP, RNU, YW
      logical KOOL
C     !DASH
      external DIVIDE, HI, BYE
C
      call HI ('ELLIS')
C     !BEG
      if(KOOL) then
        FAC = RCP
      else
        call DIVIDE (RCP,RNU,FAC)
      end if
C
      F = YW*FAC
C     !END
      call BYE ('ELLIS')
C
      return
      end
