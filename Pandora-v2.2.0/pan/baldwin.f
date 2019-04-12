      subroutine BALDWIN
     $(WAVE,Y,LINE,I,L,SAVE)
C
C     Rudolf Loeser, 1992 Aug 04
C---- Composes a line for OSMOND.
C     !DASH
      save
C     !DASH
      real*8 WAVE, Y
      integer I, L
      character LABEL*29, LINE*63, LYBEL*10, SAVE*5
C     !DASH
      external KAKAPO, HAKO, HI, BYE
C
      call HI ('BALDWIN')
C     !BEG
      call KAKAPO (L, WAVE, WAVE, LABEL, SAVE)
      call HAKO   (Y, LYBEL)
C
      write (LINE,100) I,WAVE
  100 format(I7,1PE22.13)
C
      LINE(30:44) = LABEL(15:29)
      LINE(47:56) = LYBEL
C     !END
      call BYE ('BALDWIN')
C
      return
      end
