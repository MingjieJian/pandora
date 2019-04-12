      subroutine CUANDO
     $(N,XLM,TE,XLTR,EMM,RES)
C
C     Rudolf Loeser, 2002 Sep 18
C---- Computes resonance broadening estimate for Hydrogen Lyman N/1 line
C     wing opacity.
C     (This is version 2 of CUANDO.)
C     !DASH
      save
C     !DASH
      real*8 C1, C2, EMM, ONE, R, RES, T, TE, W, WAVE, XLM, XLTR
      integer N
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
      data WAVE,C1,C2 /1620.D0, 4.71D-2, 7.43D2/
C
      call HI ('CUANDO')
C     !BEG
      if((XLM.gt.WAVE).and.(N.eq.2)) then
        T = ONE+C2/TE
        W = XLM-WAVE
        R = -C1*W*T
C
        EMM = exp(R)
      else
        EMM = ONE
      end if
C
      RES = (ONE+XLTR)*EMM
C     !END
      call BYE ('CUANDO')
C
      return
      end
