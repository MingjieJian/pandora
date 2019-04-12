      subroutine GATE
     $(CPJ,XNUJ,RJ,R,X,UJ,KOOL, RLM,F)
C     Rudolf Loeser, 1979 Nov 26
C---- Computes the 1/nu**3 contribution to RL.
C     !DASH
      save
C     !DASH
      real*8 CPJ, E1, F, R, RJ, RLM, UJ, X, XNUJ, dummy
      logical KOOL
C     !DASH
      external FRIGOR, EXPINT, HI, BYE
C
      call HI ('GATE')
C     !BEG
      call FRIGOR (CPJ, XNUJ, KOOL, F)
      call EXPINT (1, (UJ*X), E1, dummy)
      RLM = F*RJ*R*(X**3)*E1
C     !END
      call BYE ('GATE')
C
      return
      end
