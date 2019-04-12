      subroutine SPEY
     $(X,IX,W,IW)
C
C     Rudolf Loeser, 1988 Jul 29
C---- Does regular run.
C     !DASH
      save
C     !DASH
      real*8 W, X
      integer IW, IX
C     !DASH
      external TYNE, FARRAR, RIBBLE, HI, BYE
C
      dimension X(*), IX(*), W(*), IW(*)
C
      call HI ('SPEY')
C     !BEG
C---- Iterative processing
      call TYNE   (X,IX,W,IW)
C---- Post processing
      call FARRAR (X,IX,W,IW)
C---- Spectrum processing
      call RIBBLE (X,IX,W,IW)
C     !END
      call BYE ('SPEY')
C
      return
      end
