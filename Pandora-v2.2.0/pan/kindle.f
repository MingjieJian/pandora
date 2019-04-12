      subroutine KINDLE
     $(KURU,KILROY,WAVE,WT,STEP,LU,I,KUDNT)
C
C     Rudolf Loeser, 1980 Aug 26
C---- Reads and dumps raw Statistical Line Opacity data.
C     (This is version 2 of KINDLE.)
C     !DASH
      save
C     !DASH
      real*8 STEP, WAVE, WT
      integer I, KUDNT, KURU, LU
      logical KILROY
C     !DASH
      external PILAR, HI, BYE
C
      dimension WT(10), STEP(11,9,10)
C
      call HI ('KINDLE')
C     !BEG
      read (KURU,100) WAVE,WT,STEP
  100 format(4E20.12)
C
      call PILAR (LU, KILROY, WAVE, WT, STEP, I, KUDNT)
C     !END
      call BYE ('KINDLE')
C
      return
      end
