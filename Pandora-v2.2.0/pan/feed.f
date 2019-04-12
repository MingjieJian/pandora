      subroutine FEED
     $(YL,YSML,YBIG,YM,YU,YY)
C
C     Rudolf Loeser, 1982 Apr 22
C---- Does mapping, for HEEL.
C     !DASH
      save
C     !DASH
      real*8 BIAS, FRAC, ONE, PLOT, SPAN, YBIG, YL, YM, YSML, YU, YY
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external HI, BYE
C
      call HI ('FEED')
C     !BEG
      SPAN = YBIG-YSML
      PLOT = YU-YM
      BIAS = PLOT+ONE
      FRAC = (YL-YSML)/SPAN
C
      YY = BIAS+PLOT*FRAC
C     !END
      call BYE ('FEED')
C
      return
      end
