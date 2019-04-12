      subroutine SAUCER
     $(X,W)
C
C     Rudolf Loeser, 1982 May 28
C---- Post-read defaults for Batch 4.
C     (This is version 3 of SAUCER.)
C     !DASH
      save
C     !DASH
      real*8 W, X
C     !DASH
      external HI, BYE
C
      dimension X(*), W(*)
C
C
      call HI ('SAUCER')
C     !BEG
C---- (Dummy, for the time being)
C     !END
      call BYE ('SAUCER')
C
      return
      end
