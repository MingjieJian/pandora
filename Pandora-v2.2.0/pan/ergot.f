      subroutine ERGOT
     $(TE,WAVER,WAVEJ,PIR,PIJ,PRAT)
C
C     Rudolf Loeser, 1978 Sep 20
C---- Computes an LTE population ratio.
C     (This is version 3 of ERGOT.)
C     !DASH
      save
C     !DASH
      real*8 EX, PIJ, PIR, PRAT, TE, WAVEJ, WAVER, ZJ, ZR
C     !DASH
      external HUNK, DIVIDE, HI, BYE
C
      call HI ('ERGOT')
C     !BEG
      call HUNK   (TE, WAVEJ, 2, ZJ)
      call HUNK   (TE, WAVER, 2, ZR)
      EX = exp(ZJ-ZR)
C
      call DIVIDE (PIJ, PIR, PRAT)
      PRAT = PRAT*EX
C     !END
      call BYE ('ERGOT')
C
      return
      end
