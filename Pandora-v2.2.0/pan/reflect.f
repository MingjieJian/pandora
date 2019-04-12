      subroutine REFLECT
     $(XLM,HND,ABSX)
C
C     Rudolf Loeser, 1985 Aug 09
C---- Computes X-ray Absorption.
C     (This is version 2 of REFLECT.)
C     !DASH
      save
C     !DASH
      real*8 ABSX, C0, C1, C2, E, FAC, HND, T, XLM, XLMS
C     !DASH
      external MEGARA, HI, BYE
C
      data FAC /1.D-24/
      data XLMS, E, C0, C1, C2 /5*0.D0/
C
      call HI ('REFLECT')
C     !BEG
      if(XLM.ne.XLMS) then
        XLMS = XLM
        call MEGARA (XLMS, E, C0, C1, C2)
        T = FAC*((C0+E*(C1+E*C2))/(E**3))
      end if
C
      ABSX = HND*T
C     !END
      call BYE ('REFLECT')
C
      return
      end
