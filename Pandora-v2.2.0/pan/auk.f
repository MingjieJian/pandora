      subroutine AUK
     $(ZRAW,MU,ML,N1U,N1L)
C
C     Rudolf Loeser, 1992 Jan 24
C---- Unpacks indices MU, ML, N1U, N1L, for TABOR.
C     (This is version 2 of AUK.)
C     !DASH
      save
C     !DASH
      real*8 F, F3, F6, F9, T, ZRAW
      integer ML, MU, N1L, N1U
C     !DASH
      external HI, BYE
C
      data F3,F6,F9 /1.D3, 1.D6, 1.D9/
C
      call HI ('AUK')
C     !BEG
      T = ZRAW
      MU = T/F9
C
      F = MU
      T = T-F*F9
      ML = T/F6
C
      F = ML
      T = T-F*F6
      N1U = T/F3
C
      F = N1U
      N1L = T-F*F3
C     !END
      call BYE ('AUK')
C
      return
      end
