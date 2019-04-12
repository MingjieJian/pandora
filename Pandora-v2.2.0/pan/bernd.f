      subroutine BERND
     $(POINTS,NP,SAMPLE,NS,JS,JE)
C
C     Rudolf Loeser, 1983 Feb 04
C---- Computes sample limits, for "Eclipse" plots.
C     !DASH
      save
C     !DASH
      real*8 POINTS, SAMPLE, ZL, ZU
      integer J, JE, JS, NP, NS
C     !DASH
      external HI, BYE
C
C               POINTS(NP), SAMPLE(NS)
      dimension POINTS(*),  SAMPLE(*)
C
      call HI ('BERND')
C     !BEG
      ZL = POINTS(1)
      JS = 0
      do 100 J = 1,NS
        JS = JS+1
        if(SAMPLE(JS).ge.ZL) then
          goto 101
        end if
  100 continue
C
  101 continue
C
      ZU = POINTS(NP)
      JE = NS+1
      do 102 J = 1,NS
        JE = JE-1
        if(SAMPLE(JE).le.ZU) then
          goto 103
        end if
  102 continue
C
  103 continue
C     !END
      call BYE ('BERND')
C
      return
      end
